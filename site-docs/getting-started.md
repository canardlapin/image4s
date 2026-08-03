# Getting started

This workflow builds a two-dimensional image sampled at three time points,
selects the sample at 0.5 seconds, crops it exactly, and applies a Gaussian
blur. Spatial rank, units, coordinate convention, transform policy, and time
coordinates stay visible even though the constructor plumbing is compact.

```scala mdoc:silent
import image4s.prelude.*
import image4s.filter.gaussianBlurSamples
import ravel.NDArray
```

## 1. Describe the sampling

`SamplingSpec` is a create-only request. `D2` fixes spatial rank. The frame
uses millimetres and RAS coordinates. `GridSpec.identity` states that spatial
indices map directly into that frame; use `GridSpec.axisAligned` or
`GridSpec.affine` when that transform policy is not correct.

```scala mdoc:silent
val sampling =
  SamplingSpec[D2](
    frame = FrameSpec.named(
      "scanner-plane",
      unit = LengthUnit.Millimeter,
      convention = CoordinateConvention.RAS
    ),
    grid = GridSpec.identity,
    axes = AxesSpec(
      AxisSpec.timeRegular(
        origin = 0.0,
        step = 0.5,
        unit = AxisUnit.Seconds
      )
    )
  )
```

The time extent is intentionally absent from `AxisSpec.timeRegular`: the Ravel
shape binds it when the image is built. This prevents a declared axis extent
from drifting away from the data shape.

## 2. Attach values and take exact views

The Ravel shape is the spatial shape followed by declared non-spatial axes:
`6 × 5 × 3`. Construction, coordinate lookup, time selection, and crop form
one `Either[ImageError, ...]` workflow.

```scala mdoc:silent
val values =
  NDArray.tabulate[Double](6, 5, 3) { (x, y, t) =>
    x.toDouble + 10.0 * y + 100.0 * t
  }

val imageWorkflow =
  for
    image <- Image.continuous(values, sampling)
    coordinate <- image.nonSpatialAxes.coordinateAt(axis = 0, index = 1)
    atHalfSecond <- image.atTime(1)
    crop <- atHalfSecond.crop(
      origin = Vector(1, 1),
      shape = Vector(4, 3)
    )
  yield (image, coordinate, atHalfSecond, crop)

val (image, selectedCoordinate, atHalfSecond, crop) =
  imageWorkflow.fold(
    error => throw new IllegalArgumentException(error.message),
    identity
  )

assert(image.logicalShape == Vector(6, 5, 3))
assert(selectedCoordinate == AxisCoordinate.Numeric(0.5, AxisUnit.Seconds))
assert(atHalfSecond.logicalShape == Vector(6, 5))
assert(atHalfSecond(2, 3) == image(2, 3, 1))
assert(crop.logicalShape == Vector(4, 3))
assert(crop(0, 0) == atHalfSecond(1, 1))
assert(crop.grid.indexToFrame(Vector(0.0, 0.0)) ==
  atHalfSecond.grid.indexToFrame(Vector(1.0, 1.0)))
```

`atTime(1)` selects by index from the sole declared time axis. It does not
silently search for a numeric coordinate. Coordinate-value selection is left
explicit until a caller chooses exact, tolerance-bound, or nearest matching.

The crop is a zero-copy storage view. Its grid changes shape and index origin
so index `(0, 0)` still names the physical location sampled at `(1, 1)` in the
source.

## 3. State the Gaussian coordinate system

`gaussianBlurSamples(0.8)` measures sigma in sample-index steps. Use
`gaussianBlurFrame(value, LengthUnit...)` when sigma is stated in frame
coordinates. Filtering computes new values, so it is a separate `Either`
boundary from the exact-view workflow.

```scala mdoc:silent
val blurred =
  crop
    .gaussianBlurSamples(0.8)
    .fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

assert(blurred.logicalShape == crop.logicalShape)
assert(blurred.grid.sameRuntimeOwnerAs(crop.grid))
```

```scala mdoc
(selectedCoordinate, crop.logicalShape, blurred.logicalShape)
```

## What the façade does—and does not do

`Image.continuous(values, sampling)` expands the specification into the
ordinary `Frame`, `Grid`, `Axis`, `NonSpatialAxes`, and `SampleSpace` values,
then runs the canonical image constructor. The image retains only those
canonical values and the original Ravel array; it does not retain a second
geometry model or copy storage.

| Step | Values | Spatial grid | Time axis | Semantic role |
| --- | --- | --- | --- | --- |
| Construct | Original array | Declared `6 × 5` grid | Three declared coordinates | Continuous |
| `atTime(1)` | Exact view | Same live grid | Removed | Continuous |
| `crop(...)` | Exact view | Affine-correct `4 × 3` crop grid | Absent | Continuous |
| `gaussianBlurSamples(...)` | Newly computed | Same as crop with `Same` extent | Absent | Continuous |

The prelude exports core image and geometry vocabulary plus Ravel dtype givens.
It deliberately does not export `NDArray`, filtering, NIfTI, Intaglio, locus,
or reframe4s APIs; import those from their owning modules.

For persistent frame identity, shared live owners, or a custom affine built in
stages, continue to [Geometry and sampled axes](understand/geometry-and-axes.md).
For storage-only transformations, continue to
[Select and transform exact views](work/exact-views.md).
