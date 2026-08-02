# Python parity and performance benchmark

The repository includes a reproducible comparison between representative
image4s operations and SciPy operations. It checks numerical outputs on fixed
D2 and D3 fixtures, then records in-process wall-clock medians for the same
operation families.

## Run it

The harness requires Python 3 with NumPy and SciPy. From the repository root:

```sh
python3 tools/run_image_ops_parity.py
```

The command also runs the JVM benchmark through sbt. To adjust timing effort:

```sh
python3 tools/run_image_ops_parity.py --warmups 5 --iterations 15 --inner-repetitions 3
```

To measure an edited local Ravel checkout explicitly, pass its absolute or
repository-relative path:

```sh
python3 tools/run_image_ops_parity.py --ravel-build ../ravel
```

The wrapper pins `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, and
`MKL_NUM_THREADS` to one by default. Override that deliberately when measuring
a configured threaded deployment:

```sh
python3 tools/run_image_ops_parity.py --threads 4
```

The generated files are written under `target/image-ops-parity/`:

- `report.md` contains the parity errors and timing ratios;
- `python-parity.tsv` and `scala-parity.tsv` contain the flattened reference
  outputs;
- `python-benchmark.tsv` and `scala-benchmark.tsv` contain timing samples and
  median, p25, p75, min, max, and median-absolute-deviation values. Scala rows
  are tagged `one-shot`, `prepare`, or `prepared-run`;
- the environment files record Python, NumPy, SciPy, JVM, Scala, and host
  details.

The Scala entry point can also be run directly with
`sbt imageOpsParityJVM`, but the Python wrapper is the reproducible comparison
command because it runs both implementations and applies the tolerances.

## Operation mapping

| image4s operation | SciPy reference | Boundary and representation |
| --- | --- | --- |
| D2/D3 Gaussian | `scipy.ndimage.gaussian_filter` | `float64`, constant zero border, fixed sigma and `truncate=3` |
| D2 correlation | `scipy.ndimage.correlate` | explicit asymmetric 3x3 kernel, constant zero border |
| D2/D3 Sobel | `scipy.ndimage.correlate1d` | image4s derivative `[-0.5, 0, 0.5]` and smoothing `[0.25, 0.5, 0.25]` |
| D2 cross dilation | `scipy.ndimage.binary_dilation` | threshold at `0.5`, cross support, false border |
| D3 ball dilation | `scipy.ndimage.binary_dilation` | threshold at `0.5`, six-neighbour ball support, false border |

The Sobel reference uses `correlate1d` with image4s's explicit normalized
kernels. SciPy's convenience `sobel` function uses a different scale, so it
would not be the same operation for this comparison.

The parity fixtures use non-square and non-cubic shapes. The benchmark court
also includes a square/cubic workload and separate `(192, 96)` and
`(24, 32, 40)` workloads, so a transposed axis or an incorrect D3 storage
order changes either correctness or the shape-scaled timing profile. Outputs
must be finite and must satisfy `rtol=5e-11` and `atol=5e-12` by default.

## Interpreting performance

Each timing row measures repeated calls in the same process after a short
warm-up. `--inner-repetitions` repeats the operation inside one clock sample
and reports the per-call time, reducing timer-resolution noise for small
operations. The cross-language table compares only `one-shot` rows, where
Gaussian, correlation, gradient, threshold, and morphology setup is included
on every timed call in both implementations. The Scala artifact separately
measures preparation and prepared-plan throughput; those phases are not
presented as SciPy comparisons because the public APIs do not expose the same
plan contract.

The report's ratio is `Python median / Scala median`; a value above one means
Scala was faster for that case on that machine. This is a baseline, not a
portable performance guarantee. Repeat it on the same host, with the same
versions and thread settings, before drawing a performance conclusion. The
SciPy side may call compiled native kernels, while the image4s side runs on the
JVM, so the report should be read as an end-to-end implementation comparison.

Julia 1.12 is present in the development environment, but the repository does
not currently have a Julia image-filtering package pinned. The harness therefore
uses SciPy as the external oracle and does not install Julia packages as a
benchmark side effect.
