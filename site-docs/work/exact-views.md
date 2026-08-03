# Select and transform exact views

An exact view selects or rearranges samples that already exist. image4s changes
the grid or declared axes with the Ravel view so the result remains a sampled
image rather than an anonymous array slice.

The examples below use a D2 grid with time and channel axes.

```scala mdoc:silent
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray

def checked[A](value: Either[?, A]): A =
  value match
    case Right(result) => result
    case Left(error)   => throw new IllegalArgumentException(error.toString)

val frame = checked(Frame.named[D2]("view-plane"))
val grid = checked(Grid.in(frame)(Vector(4, 3), Affine.identity[D2]))
val time =
  checked(
    Axis.regular(
      "time",
      AxisKind.Time,
      extent = 2,
      origin = 0.0,
      step = 1.0,
      unit = AxisUnit.Seconds
    )
  )
val channel =
  checked(
    Axis.categorical(
      "channel",
      AxisKind.Channel,
      Vector("magnitude", "phase")
    )
  )
val axes = checked(NonSpatialAxes.from(Vector(time, channel)))
val values =
  NDArray.tabulate[Double](4, 3, 2, 2) { (x, y, t, c) =>
    x + 10.0 * y + 100.0 * t + 1000.0 * c
  }
val image = checked(Image.continuous(grid, axes, values))
```

## Select a declared coordinate

`atTime` finds the unique axis whose kind is `Time`. It rejects a missing or
repeated time axis instead of choosing a trailing position by convention.

```scala mdoc:silent
val atOneSecond = checked(image.atTime(1))
val (coordinate, selectedChannel) =
  checked(image.selectNonSpatialWithCoordinate(axis = 1, index = 0))

assert(atOneSecond.logicalShape == Vector(4, 3, 2))
assert(coordinate == AxisCoordinate.Categorical("magnitude"))
assert(selectedChannel.logicalShape == Vector(4, 3, 2))
```

Use `selectNonSpatial(axis, index)` when the axis position is already known.
Use `selectNonSpatialWithCoordinate` when downstream code or a receipt must
retain the declared coordinate that was selected.

## Change the spatial lattice exactly

```scala mdoc:silent
val crop = checked(image.crop(Vector(1, 0), Vector(2, 3)))
val flipped = checked(image.flipSpatial(axis = 0))
val transposed = checked(image.permuteSpatial(Vector(1, 0)))
val strided = checked(image.strideSpatial(Vector(2, 1)))

assert(crop.logicalShape == Vector(2, 3, 2, 2))
assert(crop(0, 0, 0, 0) == image(1, 0, 0, 0))
assert(flipped(0, 0, 0, 0) == image(3, 0, 0, 0))
assert(transposed.logicalShape == Vector(3, 4, 2, 2))
assert(strided.logicalShape == Vector(2, 3, 2, 2))
```

- `crop` translates the target grid origin.
- `flipSpatial` reflects one grid axis and adjusts its affine translation.
- `permuteSpatial` reorders spatial shape and affine columns together.
- `strideSpatial` changes the sample spacing represented by the affine.

These operations accept integer-affine lattice maps. They do not pretend that
an arbitrary rotation, shear, or nonlinear transform is a storage view.

## Reorder declared axes

`permuteNonSpatial` reorders both the axis declarations and the matching Ravel
axes.

```scala mdoc:silent
val channelThenTime = checked(image.permuteNonSpatial(Vector(1, 0)))

assert(channelThenTime.nonSpatialAxes.records ==
  Vector(channel.record, time.record))
assert(channelThenTime(1, 2, 0, 1) == image(1, 2, 1, 0))
```

## Materialize only when a consumer needs it

Views can have non-canonical strides. `canonicalLayout` returns the same image
when its data is already canonical and otherwise copies logical values into a
canonical buffer. `materializedCopy` always makes that copy.

```scala mdoc:silent
val canonical = flipped.canonicalLayout
val independentCopy = crop.materializedCopy

assert(canonical.data.isCanonicalLayout)
assert(independentCopy.data.isCanonicalLayout)
assert(independentCopy.sameValuesAs(crop)(_ == _))
```

## What changed?

| Operation | Values | Spatial grid | Non-spatial axes | Semantic role |
| --- | --- | --- | --- | --- |
| `atTime` / `selectNonSpatial` | Exact view | Same live grid | One removed | Preserved |
| `crop` | Exact view | Shape and affine origin changed | Preserved | Preserved |
| `flipSpatial` | Exact view | One affine direction reflected | Preserved | Preserved |
| `permuteSpatial` | Exact view | Shape and affine columns reordered | Preserved | Preserved |
| `strideSpatial` | Exact view | Shape and affine spacing changed | Preserved | Preserved |
| `permuteNonSpatial` | Exact view | Same live grid | Reordered with data | Preserved |
| `canonicalLayout` | Same logical values; copy only if needed | Same live grid | Preserved | Preserved |
| `materializedCopy` | Copied | Same live grid | Preserved | Preserved |

> A view selects or rearranges existing samples exactly. Resampling computes
> values on another grid.

Use reframe4s when the target is not an exact view. Continue to
[Combine aligned images](combine-aligned-images.md) when two sampled values
must participate in one pointwise operation.
