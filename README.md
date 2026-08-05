# image4s

[Guide](https://canardlapin.github.io/image4s/) · [Guide source](site-docs/README.md) · [CI workflow](.github/workflows/ci.yml) · [Representation contract](docs/image-representation-contract.md) · [Versioning policy](docs/versioning-policy.md)

image4s is a typed Scala 3 library for multidimensional images that keeps
values aligned with their spatial geometry, declared non-spatial sampling
axes, metadata, and value semantics. Use it for scientific or data-processing
pipelines where a crop, slice, or view must preserve what the values mean.
The core API is cross-compiled for the JVM and Scala.js.

> **Status:** Early development on `0.1.0-SNAPSHOT`. APIs and module
> boundaries may change; this is not a stable support commitment.

## Quick start

The current project is sbt-first and built from source. From a checkout:

```text
git clone https://github.com/canardlapin/image4s.git
cd image4s
sbt -batch image4sCompileAll
```

This constructs a 3D image with a declared time axis, selects one time point,
takes a spatial crop, and maps its values:

```scala
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray

val frame =
  Frame.named[D3](
    "native",
    unit = LengthUnit.Millimeter,
    convention = CoordinateConvention.RAS
  ).toOption.get
val grid =
  Grid.in(frame)(Vector(6, 7, 5), Affine.identity[D3]).toOption.get
val time =
  Axis.regular(
    "time",
    AxisKind.Time,
    extent = 4,
    origin = 0.0,
    step = 0.8,
    unit = AxisUnit.Seconds
  ).toOption.get
val axes = NonSpatialAxes.from(Vector(time)).toOption.get
val values =
  NDArray.tabulate[Double](6, 7, 5, 4)((i, j, k, t) =>
    1000.0 * i + 100.0 * j + 10.0 * k + t
  )

val image = Image.continuous(grid, axes, values).toOption.get
val volume = image.atTime(2).toOption.get
val crop =
  volume
    .crop(origin = Vector(1, 2, 1), shape = Vector(3, 4, 2))
    .toOption
    .get
val centered = crop.mapValues(_ - 1000.0)

assert(image.logicalShape == Vector(6, 7, 5, 4))
assert(centered(0, 0, 0) == 212.0)
```

The example unwraps successful `Either` values to keep the workflow visible;
constructors and operations return typed errors, which application code should
handle explicitly. The same construction, selection, crop, and map workflow is
compiled and run on both platforms by
[`ApproachableApiSuite`](modules/image4s-core/shared/src/test/scala/image4s/ApproachableApiSuite.scala):

```text
sbt -batch "image4s-coreJVM / Test / testOnly image4s.ApproachableApiSuite" "image4s-coreJS / Test / testOnly image4s.ApproachableApiSuite"
```

## What it covers

- Keep a spatial grid and ordered time, channel, echo, categorical, or custom
  axes attached to immutable Ravel-backed values.
- Select axes, crop, flip, permute, or stride through checked views when the
  requested transformation is an exact Ravel view.
- Make value semantics, interpolation, numeric storage conversion, and filter
  output types explicit.
- Apply convolution, correlation, Gaussian filtering, thresholding, and
  binary or grayscale morphology.
- Read and write the supported scalar NIfTI-1 subset with explicit scaling,
  label, affine, and I/O policies.
- Run the core image model on the JVM and Scala.js; the Scala.js NIfTI file
  adapter targets Node.js rather than browser filesystem APIs.

## Fit and boundaries

image4s is a good fit when image meaning is part of correctness: a value should
not silently lose its grid, sampling coordinates, semantic role, or storage
policy while it moves through a pipeline.

It is not a general tensor library or a registration/resampling engine. Ravel
owns dense storage, layouts, views, and numeric kernels; image4s owns sampled
image semantics and geometry; reframe4s owns production transformations and
resampling; locus4s owns identity-safe finite domains. `image4s-core` has no
filesystem, format, or filter catalogue API.

NIfTI support is deliberately bounded to scalar NIfTI-1 UInt8, Int16, Int32,
Float32, and Float64 payloads plus the implemented endianness, gzip, extension,
and affine cases. Complex, RGB, binary, wider-integer, and NIfTI-2 payloads
are rejected rather than guessed.

## Modules

Start with `image4s-core`; add only the modules needed by the workflow:

| Module | Use it for |
|---|---|
| `image4s-geometry` | dimensions, frames, points, vectors, grids, identities, and affine coordinates |
| `image4s-core` | `Sampled`, axes, value semantics, checked construction, metadata, and views |
| `image4s-filter` | convolution, correlation, Gaussian filtering, and neighborhood filters |
| `image4s-morphology` | binary and grayscale morphology |
| `image4s-nifti` | NIfTI-1 parsing and encoding, with JVM and Node.js adapters |
| `image4s-reference` | independent nearest and linear sampling oracles |
| `image4s-locus` | checked conversion from an image grid to a locus4s finite domain |
| `image4s-intaglio` | display-only lowering of D2 continuous images into Intaglio fields and rasters |

`image4s-ops-core`, `image4s-laws`, and `image4s-ops-laws` provide shared
operation vocabulary, law suites, visual QA, parity benchmarks, and allocation
checks for development and validation.

## Semantics that affect use

- **Image meaning stays together.** `Sampled` combines the Ravel `NDArray`, a
  validated spatial grid, ordered non-spatial axes, metadata, and value
  semantics. Ravel's physical storage order is not used as a substitute for
  spatial meaning.
- **Axes carry sampling meaning.** Use `Axis.regular`, `Axis.explicit`, or
  `Axis.categorical` when coordinates matter. Logical axes are spatial axes
  followed by the declared non-spatial axes.
- **Views are exact or explicit.** `LatticeMap` represents only signed
  integer-affine maps and axis permutations that Ravel can express as a
  zero-copy view. Broader transforms must be materialized or resampled
  explicitly.
- **Alignment is not guessed.** Images built from one live `SampleSpace` can
  combine directly. Independently reconstructed spaces require an explicit
  `SamplingAlignment`; structural similarity does not create hidden ownership.
- **Numeric behavior is visible.** `Sampled.convertTo[B]` takes an explicit
  conversion policy. Floating images can use `gaussianBlur`; integer-backed
  continuous images must request a floating output with `gaussianBlurTo[B]`
  rather than silently truncating.
- **NIfTI scaling is visible.** Stored codes, scaled values, labels, affine
  choice, and I/O limits have separate policies and typed failure paths.

The [canonical image representation contract](docs/image-representation-contract.md)
is the normative description of these boundaries.

## Documentation and evidence

Start with the [published reader-facing guide](https://canardlapin.github.io/image4s/),
or inspect its [Laika/mdoc source](site-docs/README.md). Build the executable
site from a checkout with:

```text
sbt -batch docs/tlSite
```

- [Canonical image representation contract](docs/image-representation-contract.md)
  — ownership, axes, geometry, identity, views, and failure behavior.
- [Sample-space owner decision](docs/sample-space-owner-decision.md) — why
  runtime sample-space ownership is part of the public model.
- [Versioning and compatibility policy](docs/versioning-policy.md) — early
  semantic versioning and the current snapshot baseline.
- [Image-operation visual QA](docs/visual-qa.md) — cross-platform operation
  conformance and visual checks.
- [Python parity benchmark](docs/parity-benchmark.md) — operation mapping and
  benchmark interpretation.
- [Ravel capability gate](docs/ravel-capability-gate.md) — evidence required
  before production resampling work can move into this layer.
- [Core API source](modules/image4s-core/shared/src/main/scala/image4s/) and
  [geometry API source](modules/image4s-geometry/shared/src/main/scala/image4s/geometry/)
  — the current public implementation surface.

## Build and test

The build declares Scala 3.7.4 and sbt 1.11.7. The configured CI matrix runs
the JVM and Scala.js suites on JDK 17 and 21, plus optimized Scala.js/Node
checks, version-policy checks, concurrency suites, and isolated allocation or
performance courts.

Compile all projects:

```text
sbt -batch image4sCompileAll
```

Run the full JVM and Scala.js test suite:

```text
sbt -batch image4sTestAll
```

For operation-specific visual checks, see [visual QA](docs/visual-qa.md). For
the Python comparison workflow, see the [parity benchmark](docs/parity-benchmark.md).

When developing against sibling source checkouts, override the pinned builds
explicitly:

```text
-Dimage4s.ravel.build=/absolute/path/to/ravel
-Dimage4s.gale.build=/absolute/path/to/gale
-Dimage4s.locus4s.build=/absolute/path/to/locus4s
```

Ordinary builds use the exact revisions declared in `build.sbt`.

## License

Apache-2.0, as declared by the build. See the
[Apache-2.0 license text](https://www.apache.org/licenses/LICENSE-2.0).
