# Getting started

This workflow builds a two-dimensional image sampled at three time points,
selects the sample at 0.5 seconds, crops it, and applies a Gaussian blur. Using
two spatial dimensions keeps the difference between spatial geometry and time
visible in the types and shapes.

The example composes operations that can fail with `Either`. Each section
converts failure to an exception once so mdoc stops if the example becomes
invalid. Application code should instead report or recover from the error.

```scala mdoc:silent
import image4s.*
import image4s.filter.gaussianBlur
import image4s.geometry.*
import image4s.ops.SpatialSigma
import ravel.DType.given
import ravel.NDArray
```

## 1. Declare a spatial grid

The grid has six samples along its first spatial axis and five along its
second. Its identity affine maps index `(x, y)` to the same coordinates in the
named frame.

```scala mdoc:silent
val frameAndGrid =
  for
    frame <- Frame.named[D2](
      "scanner-plane",
      unit = LengthUnit.Millimeter,
      convention = CoordinateConvention.RAS
    )
    grid <- Grid.in(frame)(Vector(6, 5), Affine.identity[D2])
  yield (frame, grid)

val (frame, grid) =
  frameAndGrid.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
```

## 2. Declare time coordinates

The time axis has coordinates 0.0, 0.5, and 1.0 seconds. It is not merely a
trailing array dimension.

```scala mdoc:silent
val timeAndAxes =
  for
    time <- Axis.regular(
      "time",
      AxisKind.Time,
      extent = 3,
      origin = 0.0,
      step = 0.5,
      unit = AxisUnit.Seconds
    )
    axes <- NonSpatialAxes.from(Vector(time))
  yield (time, axes)

val (time, axes) =
  timeAndAxes.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
```

## 3. Attach values

The Ravel array shape is the spatial grid shape followed by the declared time
extent: `6 × 5 × 3`.

```scala mdoc:silent
val values =
  NDArray.tabulate[Double](6, 5, 3) { (x, y, t) =>
    x.toDouble + 10.0 * y + 100.0 * t
  }

val image =
  Image.continuous(grid, axes, values).fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(image.logicalShape == Vector(6, 5, 3))
```

## 4. Select the sample at 0.5 seconds

`atTime(1)` selects index 1 from the sole declared time axis. The axis itself
reports the coordinate attached to that index.

```scala mdoc:silent
val selectedAtHalfSecond =
  for
    coordinate <- time.coordinateAt(1)
    selected <- image.atTime(1)
  yield (coordinate, selected)

val (selectedCoordinate, atHalfSecond) =
  selectedAtHalfSecond.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(selectedCoordinate == AxisCoordinate.Numeric(0.5, AxisUnit.Seconds))
assert(atHalfSecond.logicalShape == Vector(6, 5))
assert(atHalfSecond(2, 3) == image(2, 3, 1))
```

## 5. Crop in space

The crop is an exact storage view. Its grid changes shape and index origin so
that index `(0, 0)` in the result still maps to the physical location sampled
at index `(1, 1)` in the source.

```scala mdoc:silent
val crop =
  atHalfSecond
    .crop(
      origin = Vector(1, 1),
      shape = Vector(4, 3)
    )
    .fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )

assert(crop.logicalShape == Vector(4, 3))
assert(crop(0, 0) == atHalfSecond(1, 1))
assert(crop.grid.indexToFrame(Vector(0.0, 0.0)) ==
  atHalfSecond.grid.indexToFrame(Vector(1.0, 1.0)))
```

## 6. Compute blurred values

The default `Same` extent keeps the crop grid and shape. The output values are
newly computed.

```scala mdoc:silent
val blurredResult =
  for
    sigma <- SpatialSigma.samples[D2](0.8)
    blurred <- crop.gaussianBlur(sigma)
  yield blurred

val blurred =
  blurredResult.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(blurred.logicalShape == crop.logicalShape)
assert(blurred.grid.sameRuntimeOwnerAs(crop.grid))
```

```scala mdoc
(selectedCoordinate, crop.logicalShape, blurred.logicalShape)
```

## What changed?

| Step | Values | Spatial grid | Time axis | Semantic role |
| --- | --- | --- | --- | --- |
| Construct | New array | Declared `6 × 5` grid | Three declared coordinates | Continuous |
| `atTime(1)` | Exact view | Same live grid | Removed | Continuous |
| `crop(...)` | Exact view | Affine-correct `4 × 3` crop grid | Absent | Continuous |
| `gaussianBlur(...)` | Newly computed | Same as crop with `Same` extent | Absent | Continuous |

All constructors and checked operations above return `Either`. A production
workflow can compose the selection and crop without throwing:

```scala
image.atTime(1).flatMap(_.crop(Vector(1, 1), Vector(4, 3)))
```

Next, read [The sampled-image model](understand/sampled-image-model.md) to name
the parts that made these guarantees possible, or continue to
[Select and transform exact views](work/exact-views.md).
