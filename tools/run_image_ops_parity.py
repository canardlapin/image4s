#!/usr/bin/env python3
"""Run SciPy parity checks and produce a cross-language performance report."""

from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
from pathlib import Path

import numpy as np


ROOT = Path(__file__).resolve().parents[1]
REFERENCE = ROOT / "tools" / "image_ops_python_reference.py"


def read_tsv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream, delimiter="\t"))


def parse_shape(value: str) -> tuple[int, ...]:
    return tuple(int(part) for part in value.split(",") if part)


def read_parity(path: Path) -> dict[str, tuple[tuple[int, ...], np.ndarray]]:
    result: dict[str, tuple[tuple[int, ...], np.ndarray]] = {}
    for row in read_tsv(path):
        shape = parse_shape(row["shape"])
        values = np.fromstring(row["values"], sep=",", dtype=np.float64)
        result[row["operation"]] = (shape, values)
    return result


def compare_parity(
    python_path: Path,
    scala_path: Path,
    relative_tolerance: float,
    absolute_tolerance: float,
) -> list[dict[str, object]]:
    python_cases = read_parity(python_path)
    scala_cases = read_parity(scala_path)
    if set(python_cases) != set(scala_cases):
        missing = sorted(set(python_cases) - set(scala_cases))
        extra = sorted(set(scala_cases) - set(python_cases))
        raise RuntimeError(f"parity operation mismatch: missing={missing}, extra={extra}")

    rows: list[dict[str, object]] = []
    for name in python_cases:
        python_shape, python_values = python_cases[name]
        scala_shape, scala_values = scala_cases[name]
        if python_shape != scala_shape:
            raise RuntimeError(
                f"{name}: shape mismatch: Python={python_shape}, Scala={scala_shape}"
            )
        if python_values.shape != scala_values.shape:
            raise RuntimeError(
                f"{name}: flattened length mismatch: "
                f"Python={python_values.size}, Scala={scala_values.size}"
            )
        if not np.isfinite(python_values).all() or not np.isfinite(scala_values).all():
            raise RuntimeError(f"{name}: non-finite output in parity artifact")
        difference = np.abs(scala_values - python_values)
        denominator = np.maximum(np.abs(python_values), 1.0e-15)
        max_absolute = float(difference.max(initial=0.0))
        max_relative = float((difference / denominator).max(initial=0.0))
        passed = bool(
            np.allclose(
                scala_values,
                python_values,
                rtol=relative_tolerance,
                atol=absolute_tolerance,
            )
        )
        rows.append(
            {
                "operation": name,
                "shape": python_shape,
                "max_absolute": max_absolute,
                "max_relative": max_relative,
                "passed": passed,
            }
        )
    failed = [row["operation"] for row in rows if not row["passed"]]
    if failed:
        raise RuntimeError(f"parity tolerance failure: {failed}")
    return rows


def read_timings(path: Path, phase: str = "one-shot") -> dict[str, dict[str, str]]:
    return {
        row["operation"]: row
        for row in read_tsv(path)
        if row.get("phase", "one-shot") == phase
    }


def read_environment(path: Path) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        key, value = line.split("\t", maxsplit=1)
        values[key] = value
    return values


def git_state() -> tuple[str, bool]:
    revision = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    dirty = bool(
        subprocess.run(
            ["git", "status", "--porcelain"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()
    )
    return revision, dirty


def write_report(
    path: Path,
    parity_rows: list[dict[str, object]],
    python_timings: dict[str, dict[str, str]],
    scala_timings: dict[str, dict[str, str]],
    scala_timing_rows: list[dict[str, str]],
    python_environment: dict[str, str],
    scala_environment: dict[str, str],
    revision: str,
    dirty: bool,
    relative_tolerance: float,
    absolute_tolerance: float,
) -> None:
    lines = [
        "# image4s Python parity and performance",
        "",
        "This report compares the same deterministic fixtures and operation "
        "contracts on the same host. It is evidence, not a universal claim "
        "that one runtime is faster.",
        "",
        "## Correctness",
        "",
        f"Tolerance: `rtol={relative_tolerance:g}`, `atol={absolute_tolerance:g}`.",
        "",
        "| operation | shape | max absolute error | max relative error | status |",
        "| --- | --- | ---: | ---: | --- |",
    ]
    for row in parity_rows:
        lines.append(
            f"| {row['operation']} | {row['shape']} | "
            f"{row['max_absolute']:.3e} | {row['max_relative']:.3e} | "
            f"{'PASS' if row['passed'] else 'FAIL'} |"
        )

    lines.extend(
        [
            "",
            "## Performance",
            "",
            "Median in-process wall-clock time; lower is better. "
            "The ratio is `Python median / Scala median`, so values above 1 "
            "mean Scala was faster for that case on this run.",
            "",
            "| operation | shape | Python median (ms) | Scala median (ms) | Python/Scala ratio | Scala p25-p75 (ms) |",
            "| --- | --- | ---: | ---: | ---: | ---: |",
        ]
    )
    if set(python_timings) != set(scala_timings):
        raise RuntimeError(
            "benchmark operation mismatch: "
            f"Python={sorted(python_timings)}, Scala={sorted(scala_timings)}"
        )
    for name in python_timings:
        python_row = python_timings[name]
        scala_row = scala_timings[name]
        python_ns = float(python_row["median_ns"])
        scala_ns = float(scala_row["median_ns"])
        ratio = python_ns / scala_ns
        lines.append(
            f"| {name} | {python_row['shape']} | {python_ns / 1.0e6:.3f} | "
            f"{scala_ns / 1.0e6:.3f} | {ratio:.2f} | "
            f"{float(scala_row['p25_ns']) / 1.0e6:.3f}-"
            f"{float(scala_row['p75_ns']) / 1.0e6:.3f} |"
        )

    phase_rows = [row for row in scala_timing_rows if row.get("phase") != "one-shot"]
    if phase_rows:
        lines.extend(
            [
                "",
                "## Scala execution phases",
                "",
                "These rows decompose JVM preparation from prepared-plan throughput. "
                "They are not cross-language comparisons because SciPy's public API "
                "does not expose the same reusable plan contract.",
                "",
                "| operation | phase | median (ms) | p25-p75 (ms) | MAD (ms) |",
                "| --- | --- | ---: | ---: | ---: |",
            ]
        )
        for row in phase_rows:
            lines.append(
                f"| {row['operation']} | {row['phase']} | "
                f"{float(row['median_ns']) / 1.0e6:.3f} | "
                f"{float(row['p25_ns']) / 1.0e6:.3f}-"
                f"{float(row['p75_ns']) / 1.0e6:.3f} | "
                f"{float(row['mad_ns']) / 1.0e6:.3f} |"
            )

    lines.extend(
        [
            "",
            "## Environment",
            "",
            f"- Python: `{python_environment.get('python', 'unknown')}`",
            f"- NumPy: `{python_environment.get('numpy', 'unknown')}`",
            f"- SciPy: `{python_environment.get('scipy', 'unknown')}`",
            f"- JVM: `{scala_environment.get('java', 'unknown')}`",
            f"- Scala: `{scala_environment.get('scala', 'unknown')}`",
            f"- OS: `{scala_environment.get('os', 'unknown')}`",
            f"- Checkout: `{revision}` ({'dirty' if dirty else 'clean'})",
            f"- Native thread variables: `OMP={python_environment.get('OMP_NUM_THREADS', 'unknown')}`, "
            f"`OPENBLAS={python_environment.get('OPENBLAS_NUM_THREADS', 'unknown')}`, "
            f"`MKL={python_environment.get('MKL_NUM_THREADS', 'unknown')}`",
            "",
            "The one-shot Python and Scala rows include high-level operation "
            "setup on every timed call. The Scala artifact additionally records "
            "preparation and prepared-run phases, with p25/p75 spread and MAD "
            "so noisy measurements remain visible rather than being hidden by "
            "a single median.",
            "",
        ]
    )
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("target/image-ops-parity"),
    )
    parser.add_argument("--sbt", default="sbt")
    parser.add_argument(
        "--ravel-build",
        type=Path,
        help="use a local Ravel checkout instead of the pinned remote build",
    )
    parser.add_argument("--warmups", type=int, default=3)
    parser.add_argument("--iterations", type=int, default=9)
    parser.add_argument("--inner-repetitions", type=int, default=3)
    parser.add_argument(
        "--threads",
        type=int,
        default=1,
        help="set common native numerical-library thread variables for Python",
    )
    parser.add_argument("--rtol", type=float, default=5.0e-11)
    parser.add_argument("--atol", type=float, default=5.0e-12)
    args = parser.parse_args()
    if (
        args.warmups < 0
        or args.iterations <= 0
        or args.inner_repetitions <= 0
        or args.threads <= 0
    ):
        parser.error(
            "warmups must be non-negative, iterations, inner-repetitions, and threads "
            "must be positive"
        )

    output_dir = (ROOT / args.output_dir).resolve() if not args.output_dir.is_absolute() else args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    python_command = [
        sys.executable,
        str(REFERENCE),
        "--output-dir",
        str(output_dir),
        "--warmups",
        str(args.warmups),
        "--iterations",
        str(args.iterations),
        "--inner-repetitions",
        str(args.inner_repetitions),
    ]
    benchmark_environment = os.environ.copy()
    benchmark_environment["PYTHONDONTWRITEBYTECODE"] = "1"
    for variable in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"):
        benchmark_environment[variable] = str(args.threads)
    subprocess.run(
        python_command,
        cwd=ROOT,
        env=benchmark_environment,
        check=True,
    )

    scala_command = (
        "image4s-ops-lawsJVM / Test / runMain "
        "image4s.ops.laws.ImageOpsParityBenchmark "
        f"--parity-output {output_dir / 'scala-parity.tsv'} "
        f"--benchmark-output {output_dir / 'scala-benchmark.tsv'} "
        f"--environment-output {output_dir / 'scala-environment.tsv'} "
        f"--warmups {args.warmups} --iterations {args.iterations}"
        f" --inner-repetitions {args.inner_repetitions}"
    )
    sbt_command = [args.sbt]
    if args.ravel_build is not None:
        ravel_build = (
            ROOT / args.ravel_build
        ).resolve() if not args.ravel_build.is_absolute() else args.ravel_build
        sbt_command.append(f"-Dimage4s.ravel.build={ravel_build}")
    sbt_command.extend(["-batch", scala_command])
    subprocess.run(
        sbt_command,
        cwd=ROOT,
        env=benchmark_environment,
        check=True,
    )

    parity_rows = compare_parity(
        output_dir / "python-parity.tsv",
        output_dir / "scala-parity.tsv",
        args.rtol,
        args.atol,
    )
    revision, dirty = git_state()
    write_report(
        output_dir / "report.md",
        parity_rows,
        read_timings(output_dir / "python-benchmark.tsv"),
        read_timings(output_dir / "scala-benchmark.tsv"),
        read_tsv(output_dir / "scala-benchmark.tsv"),
        read_environment(output_dir / "python-environment.tsv"),
        read_environment(output_dir / "scala-environment.tsv"),
        revision,
        dirty,
        args.rtol,
        args.atol,
    )
    print(f"parity passed; wrote report to {output_dir / 'report.md'}")


if __name__ == "__main__":
    main()
