#!/usr/bin/env python3
"""SciPy parity oracle and performance baseline for representative image4s ops.

The fixture and kernels are intentionally explicit. This script is called by
``run_image_ops_parity.py`` and can also be run on its own to refresh the
Python-side TSV artifacts.
"""

from __future__ import annotations

import argparse
import os
import platform
import statistics
import sys
import time
from pathlib import Path
from typing import Callable

import numpy as np
import scipy
from scipy import ndimage


GAUSSIAN_SIGMA_2D = 1.25
GAUSSIAN_SIGMA_3D = 1.0
TRUNCATE = 3.0
CONSTANT_BORDER = 0.0


def fixture_2d(shape: tuple[int, int]) -> np.ndarray:
    row, column = np.indices(shape)
    return ((3 * row + 5 * column) % 29).astype(np.float64) / 29.0


def fixture_3d(shape: tuple[int, int, int]) -> np.ndarray:
    x, y, z = np.indices(shape)
    return ((x + 2 * y + 3 * z) % 31).astype(np.float64) / 31.0


def asymmetric_kernel() -> np.ndarray:
    coordinates = (-1, 0, 1)
    return np.asarray(
        [
            [
                1.0 + 0.2 * x + 0.1 * y + 0.05 * x * y
                for y in coordinates
            ]
            for x in coordinates
        ],
        dtype=np.float64,
    )


ASYMMETRIC_KERNEL = asymmetric_kernel()


def gaussian_2d(image: np.ndarray) -> np.ndarray:
    return ndimage.gaussian_filter(
        image,
        sigma=GAUSSIAN_SIGMA_2D,
        mode="constant",
        cval=CONSTANT_BORDER,
        truncate=TRUNCATE,
    )


def gaussian_3d(image: np.ndarray) -> np.ndarray:
    return ndimage.gaussian_filter(
        image,
        sigma=GAUSSIAN_SIGMA_3D,
        mode="constant",
        cval=CONSTANT_BORDER,
        truncate=TRUNCATE,
    )


def correlate_2d(image: np.ndarray) -> np.ndarray:
    return ndimage.correlate(
        image,
        ASYMMETRIC_KERNEL,
        mode="constant",
        cval=CONSTANT_BORDER,
    )


def gradient_components(
    image: np.ndarray,
    derivative: np.ndarray,
    smoothing: np.ndarray,
) -> list[np.ndarray]:
    components: list[np.ndarray] = []
    for derivative_axis in range(image.ndim):
        result = image
        for axis in range(image.ndim):
            weights = derivative if axis == derivative_axis else smoothing
            result = ndimage.correlate1d(
                result,
                weights,
                axis=axis,
                mode="constant",
                cval=CONSTANT_BORDER,
            )
        components.append(result)
    return components


def sobel_components(image: np.ndarray) -> list[np.ndarray]:
    return gradient_components(
        image,
        SOBEL_DERIVATIVE,
        SOBEL_SMOOTHING,
    )


def cross_structure_2d() -> np.ndarray:
    return np.asarray(
        ((False, True, False), (True, True, True), (False, True, False)),
        dtype=bool,
    )


CROSS_STRUCTURE_2D = cross_structure_2d()


def ball_structure_3d() -> np.ndarray:
    structure = np.zeros((3, 3, 3), dtype=bool)
    structure[1, 1, 1] = True
    structure[0, 1, 1] = True
    structure[2, 1, 1] = True
    structure[1, 0, 1] = True
    structure[1, 2, 1] = True
    structure[1, 1, 0] = True
    structure[1, 1, 2] = True
    return structure


BALL_STRUCTURE_3D = ball_structure_3d()
SOBEL_DERIVATIVE = np.asarray((-0.5, 0.0, 0.5), dtype=np.float64)
SOBEL_SMOOTHING = np.asarray((0.25, 0.5, 0.25), dtype=np.float64)


def dilate_2d(image: np.ndarray) -> np.ndarray:
    return ndimage.binary_dilation(
        threshold_2d(image),
        structure=CROSS_STRUCTURE_2D,
        border_value=0,
    )


def dilate_3d(image: np.ndarray) -> np.ndarray:
    return ndimage.binary_dilation(
        threshold_3d(image),
        structure=BALL_STRUCTURE_3D,
        border_value=0,
    )


def threshold_2d(image: np.ndarray) -> np.ndarray:
    return image >= 0.5


def threshold_3d(image: np.ndarray) -> np.ndarray:
    return image >= 0.5


def parity_cases() -> list[tuple[str, np.ndarray]]:
    image_2d = fixture_2d((17, 13))
    image_3d = fixture_3d((9, 8, 7))
    sobel_2d = sobel_components(image_2d)
    sobel_3d = sobel_components(image_3d)
    return [
        ("gaussian_d2", gaussian_2d(image_2d)),
        ("correlation_d2", correlate_2d(image_2d)),
        ("sobel_d2_x", sobel_2d[0]),
        ("sobel_d2_y", sobel_2d[1]),
        ("dilate_d2", dilate_2d(image_2d).astype(np.float64)),
        ("gaussian_d3", gaussian_3d(image_3d)),
        ("sobel_d3_x", sobel_3d[0]),
        ("sobel_d3_y", sobel_3d[1]),
        ("sobel_d3_z", sobel_3d[2]),
        ("dilate_d3", dilate_3d(image_3d).astype(np.float64)),
    ]


def benchmark_cases() -> list[tuple[str, tuple[int, ...], int, Callable[[], object]]]:
    image_2d = fixture_2d((128, 128))
    image_3d = fixture_3d((32, 32, 32))
    wide_2d = fixture_2d((192, 96))
    wide_3d = fixture_3d((24, 32, 40))

    def sobel_2d() -> object:
        return np.stack(sobel_components(image_2d), axis=0)

    def sobel_3d() -> object:
        return np.stack(sobel_components(image_3d), axis=0)

    def sobel_wide_2d() -> object:
        return np.stack(sobel_components(wide_2d), axis=0)

    def sobel_wide_3d() -> object:
        return np.stack(sobel_components(wide_3d), axis=0)

    return [
        ("gaussian_d2", image_2d.shape, image_2d.size, lambda: gaussian_2d(image_2d)),
        ("correlation_d2", image_2d.shape, image_2d.size, lambda: correlate_2d(image_2d)),
        ("sobel_d2_full", (2, *image_2d.shape), image_2d.size * 2, sobel_2d),
        ("threshold_d2", image_2d.shape, image_2d.size, lambda: threshold_2d(image_2d)),
        ("dilate_d2", image_2d.shape, image_2d.size, lambda: dilate_2d(image_2d)),
        ("gaussian_d3", image_3d.shape, image_3d.size, lambda: gaussian_3d(image_3d)),
        ("sobel_d3_full", (3, *image_3d.shape), image_3d.size * 3, sobel_3d),
        ("threshold_d3", image_3d.shape, image_3d.size, lambda: threshold_3d(image_3d)),
        ("dilate_d3", image_3d.shape, image_3d.size, lambda: dilate_3d(image_3d)),
        ("gaussian_d2_wide", wide_2d.shape, wide_2d.size, lambda: gaussian_2d(wide_2d)),
        (
            "correlation_d2_wide",
            wide_2d.shape,
            wide_2d.size,
            lambda: correlate_2d(wide_2d),
        ),
        ("sobel_d2_wide_full", (2, *wide_2d.shape), wide_2d.size * 2, sobel_wide_2d),
        (
            "threshold_d2_wide",
            wide_2d.shape,
            wide_2d.size,
            lambda: threshold_2d(wide_2d),
        ),
        ("dilate_d2_wide", wide_2d.shape, wide_2d.size, lambda: dilate_2d(wide_2d)),
        ("gaussian_d3_wide", wide_3d.shape, wide_3d.size, lambda: gaussian_3d(wide_3d)),
        ("sobel_d3_wide_full", (3, *wide_3d.shape), wide_3d.size * 3, sobel_wide_3d),
        (
            "threshold_d3_wide",
            wide_3d.shape,
            wide_3d.size,
            lambda: threshold_3d(wide_3d),
        ),
        ("dilate_d3_wide", wide_3d.shape, wide_3d.size, lambda: dilate_3d(wide_3d)),
    ]


def write_parity(path: Path, cases: list[tuple[str, np.ndarray]]) -> None:
    lines = ["operation\tshape\tvalues"]
    for name, values in cases:
        array = np.asarray(values, dtype=np.float64)
        shape = ",".join(str(value) for value in array.shape)
        flattened = ",".join(format(float(value), ".17g") for value in array.ravel(order="C"))
        lines.append(f"{name}\t{shape}\t{flattened}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def benchmark(
    name: str,
    shape: tuple[int, ...],
    samples: int,
    operation: Callable[[], object],
    warmups: int,
    iterations: int,
    inner_repetitions: int,
) -> dict[str, object]:
    for _ in range(warmups):
        for _ in range(inner_repetitions):
            operation()
    timings: list[int] = []
    for _ in range(iterations):
        started = time.perf_counter_ns()
        for _ in range(inner_repetitions):
            operation()
        elapsed = time.perf_counter_ns() - started
        timings.append(max(1, elapsed // inner_repetitions))
    ordered = sorted(timings)
    median = int(statistics.median(ordered))
    deviations = sorted(abs(value - median) for value in ordered)
    return {
        "implementation": "python-scipy",
        "phase": "one-shot",
        "operation": name,
        "shape": shape,
        "samples": samples,
        "warmups": warmups,
        "iterations": iterations,
        "inner_repetitions": inner_repetitions,
        "median_ns": median,
        "p25_ns": ordered[int((len(ordered) - 1) * 0.25)],
        "p75_ns": ordered[int((len(ordered) - 1) * 0.75)],
        "min_ns": ordered[0],
        "max_ns": ordered[-1],
        "mad_ns": deviations[int((len(deviations) - 1) * 0.5)],
    }


def write_benchmarks(
    path: Path,
    rows: list[dict[str, object]],
) -> None:
    lines = [
        "implementation\tphase\toperation\tshape\tsamples\twarmups\titerations\t"
        "inner_repetitions\tmedian_ns\tp25_ns\tp75_ns\tmin_ns\tmax_ns\tmad_ns"
    ]
    for row in rows:
        shape_text = ",".join(str(value) for value in row["shape"])
        lines.append(
            f"{row['implementation']}\t{row['phase']}\t{row['operation']}\t{shape_text}\t"
            f"{row['samples']}\t{row['warmups']}\t{row['iterations']}\t"
            f"{row['inner_repetitions']}\t{row['median_ns']}\t{row['p25_ns']}\t"
            f"{row['p75_ns']}\t{row['min_ns']}\t{row['max_ns']}\t{row['mad_ns']}"
        )
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_environment(path: Path) -> None:
    lines = [
        f"python\t{sys.version.split()[0]}",
        f"numpy\t{np.__version__}",
        f"scipy\t{scipy.__version__}",
        f"platform\t{platform.platform()}",
        f"processor\t{platform.processor()}",
        f"OMP_NUM_THREADS\t{os.environ.get('OMP_NUM_THREADS', 'unset')}",
        f"OPENBLAS_NUM_THREADS\t{os.environ.get('OPENBLAS_NUM_THREADS', 'unset')}",
        f"MKL_NUM_THREADS\t{os.environ.get('MKL_NUM_THREADS', 'unset')}",
    ]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("target/image-ops-parity"),
    )
    parser.add_argument("--warmups", type=int, default=3)
    parser.add_argument("--iterations", type=int, default=10)
    parser.add_argument("--inner-repetitions", type=int, default=3)
    args = parser.parse_args()
    if args.warmups < 0 or args.iterations <= 0 or args.inner_repetitions <= 0:
        parser.error(
            "warmups must be non-negative, iterations and inner-repetitions must be positive"
        )

    args.output_dir.mkdir(parents=True, exist_ok=True)
    write_parity(args.output_dir / "python-parity.tsv", parity_cases())
    rows = [
        benchmark(
            name,
            shape,
            samples,
            operation,
            args.warmups,
            args.iterations,
            args.inner_repetitions,
        )
        for name, shape, samples, operation in benchmark_cases()
    ]
    write_benchmarks(args.output_dir / "python-benchmark.tsv", rows)
    write_environment(args.output_dir / "python-environment.tsv")
    print(f"wrote SciPy parity and benchmark inputs to {args.output_dir}")


if __name__ == "__main__":
    main()
