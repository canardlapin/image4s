# Getting started

The smallest useful image4s workflow has four parts:

1. describe a physical frame and grid;
2. declare non-spatial sampling axes;
3. construct a `Sampled` value with matching logical shape; and
4. use checked views and value transformations.

The helper below is only for documentation. It turns a failed `Either` into
an exception so that mdoc stops the build at the source line that failed. In
application code, keep the `Either` and handle or compose the error explicitly.

```scala mdoc:silent
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray

def checked[A](value: Either[?, A]): A =
  value match
    case Right(result) => result
    case Left(error) =>
      throw new IllegalArgumentException(error.toString)

val frame =
  checked(
    Frame.named[D3](
      "native",
      unit = LengthUnit.Millimeter,
      convention = CoordinateConvention.RAS
    )
  )
val grid =
  checked(
    Grid.in(frame)(Vector(6, 7, 5), Affine.identity[D3])
  )
val time =
  checked(
    Axis.regular(
      "time",
      AxisKind.Time,
      extent = 4,
      origin = 0.0,
      step = 0.8,
      unit = AxisUnit.Seconds
    )
  )
val axes = checked(NonSpatialAxes.from(Vector(time)))
val values =
  NDArray.tabulate[Double](6, 7, 5, 4) { (i, j, k, t) =>
    1000.0 * i + 100.0 * j + 10.0 * k + t
  }
val image = checked(Image.continuous(grid, axes, values))
val volume = checked(image.atTime(2))
val crop =
  checked(
    volume.crop(
      origin = Vector(1, 2, 1),
      shape = Vector(3, 4, 2)
    )
  )
val centered = crop.mapValues(_ - 1000.0)

assert(image.logicalShape == Vector(6, 7, 5, 4))
assert(volume.logicalShape == Vector(6, 7, 5))
assert(crop.logicalShape == Vector(3, 4, 2))
assert(centered(0, 0, 0) == 212.0)
```

The values are stored with logical shape `6 × 7 × 5 × 4`: the first three
axes belong to the spatial grid and the last axis is the declared time axis.
Selecting time index `2` removes that non-spatial axis from the view. Cropping
then changes the spatial grid as well as the logical shape, so the crop remains
physically meaningful rather than becoming an anonymous array slice.

```scala mdoc
(image.logicalShape, volume.logicalShape, crop.logicalShape, centered(0, 0, 0))
```

The constructor and view operations all return typed `Either` values. The
production spelling is therefore the same workflow with explicit error
handling, for example:

```scala
image.atTime(2).flatMap(_.crop(Vector(1, 2, 1), Vector(3, 4, 2)))
```

That expression is still checked: an unknown time axis, an out-of-range
index, or an invalid crop is a value-level failure rather than an unchecked
array exception.
