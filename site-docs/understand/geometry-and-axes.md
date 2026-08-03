# Geometry and sampled axes

Four objects describe where image samples belong. Each has one job:

| Object | Job |
| --- | --- |
| `Frame` | Name the coordinate system and its physical conventions |
| `Grid` | Give the finite spatial shape and index-to-frame affine |
| `Axis` | Declare coordinates for one non-spatial dimension |
| `SampleSpace` | Bind one grid to an ordered collection of axes |

## A frame names the coordinate system

A frame records spatial rank, length unit, and coordinate convention. Two
coordinate vectors with the same numbers are not interchangeable when they
belong to different frames.

## A grid maps indices to frame points

This affine gives each sample a spacing of 2 millimetres and places index
`(0, 0)` at frame coordinate `(10, 20)`.

```scala mdoc:silent
import image4s.*
import image4s.geometry.*

val geometry =
  for
    frame <- Frame.named[D2](
      "scanner-plane",
      unit = LengthUnit.Millimeter,
      convention = CoordinateConvention.RAS
    )
    affine <- Affine.fromRowMajor[D2](
      Vector(
        2.0, 0.0, 10.0,
        0.0, 2.0, 20.0,
        0.0, 0.0, 1.0
      )
    )
    grid <- Grid.in(frame)(Vector(6, 5), affine)
    index <- grid.index(2, 1)
    point <- grid.pointAt(index)
  yield (frame, grid, point)

val (frame, grid, point) =
  geometry.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(point.coordinates == Vector(14.0, 22.0))
```

```scala mdoc
point.coordinates
```

The first array dimension follows the first grid index axis. That storage order
does not assert that the axis points left, right, anterior, or superior. The
affine and frame convention determine spatial direction.

## Axes declare non-spatial coordinates

Use the constructor that matches the sampling information you actually have.

```scala mdoc:silent
val declaredAxes =
  for
    time <- Axis.regular(
      "time",
      AxisKind.Time,
      extent = 3,
      origin = 0.0,
      step = 0.5,
      unit = AxisUnit.Seconds
    )
    echo <- Axis.explicit(
      "echo",
      AxisKind.Echo,
      values = Vector(0.012, 0.028, 0.044),
      unit = AxisUnit.Seconds
    )
    channel <- Axis.categorical(
      "channel",
      AxisKind.Channel,
      labels = Vector("magnitude", "phase")
    )
  yield (time, echo, channel)

val (time, echo, channel) =
  declaredAxes.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(time.coordinateAt(1) ==
  Right(AxisCoordinate.Numeric(0.5, AxisUnit.Seconds)))
assert(echo.coordinateAt(2) ==
  Right(AxisCoordinate.Numeric(0.044, AxisUnit.Seconds)))
assert(channel.coordinateAt(0) ==
  Right(AxisCoordinate.Categorical("magnitude")))
```

Regular axes store an origin and step. Explicit axes retain each numeric
coordinate. Categorical axes retain labels. An ordinal axis is available when
only position is known; it does not pretend to know a physical coordinate.

## Axis order is part of the sample space

```scala mdoc:silent
val axes =
  NonSpatialAxes
    .from(Vector(time, channel))
    .fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )
val space = SampleSpace.create(grid, axes)

assert(space.logicalShape == Vector(6, 5, 3, 2))
assert(space.nonSpatialAxes.records == Vector(time.record, channel.record))
```

Here array axis 2 means time and axis 3 means channel because the
`NonSpatialAxes` value declares that order. Reversing the declarations creates
a different sampling description even when the two extents happen to match.

## Checked failures protect the description

Frames reject invalid names or conventions, grids reject rank and affine
mismatches, and axes reject empty extents, non-finite coordinates, zero regular
steps, or invalid labels. These constructors return `Either` so a caller can
report the bad declaration before attaching values.

Next, use these objects in
[Select and transform exact views](../work/exact-views.md).
