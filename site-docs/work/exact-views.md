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

val setup =
  for
    frame <- Frame.named[D2]("view-plane")
    grid <- Grid.in(frame)(Vector(4, 3), Affine.identity[D2])
    time <- Axis.regular(
      "time",
      AxisKind.Time,
      extent = 2,
      origin = 0.0,
      step = 1.0,
      unit = AxisUnit.Seconds
    )
    channel <- Axis.categorical(
      "channel",
      AxisKind.Channel,
      Vector("magnitude", "phase")
    )
    axes <- NonSpatialAxes.from(Vector(time, channel))
    values = NDArray.tabulate[Double](4, 3, 2, 2) { (x, y, t, c) =>
      x + 10.0 * y + 100.0 * t + 1000.0 * c
    }
    image <- Image.continuous(grid, axes, values)
  yield (frame, grid, time, channel, image)

val (frame, grid, time, channel, image) =
  setup.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
```

## Select a declared coordinate

`atTime` finds the unique axis whose kind is `Time`. It rejects a missing or
repeated time axis instead of choosing a trailing position by convention.

```scala mdoc:silent
val selections =
  for
    atOneSecond <- image.atTime(1)
    coordinate <- image.nonSpatialAxes.coordinateAt(axis = 1, index = 0)
    selectedChannel <- image.selectNonSpatial(axis = 1, index = 0)
  yield (atOneSecond, coordinate, selectedChannel)

val (atOneSecond, coordinate, selectedChannel) =
  selections.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(atOneSecond.logicalShape == Vector(4, 3, 2))
assert(coordinate == AxisCoordinate.Categorical("magnitude"))
assert(selectedChannel.logicalShape == Vector(4, 3, 2))
```

Use `selectNonSpatial(axis, index)` when the axis position is already known.
Query `nonSpatialAxes.coordinateAt(axis, index)` as well when downstream code
or a receipt must retain the declared coordinate that was selected.

## Change the spatial lattice exactly

```scala mdoc:silent
val spatialViews =
  for
    crop <- image.crop(Vector(1, 0), Vector(2, 3))
    flipped <- image.flipSpatial(axis = 0)
    transposed <- image.permuteSpatial(Vector(1, 0))
    strided <- image.strideSpatial(Vector(2, 1))
  yield (crop, flipped, transposed, strided)

val (crop, flipped, transposed, strided) =
  spatialViews.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

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
val channelThenTime =
  image
    .permuteNonSpatial(Vector(1, 0))
    .fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )

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
