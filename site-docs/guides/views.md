# Views and coordinates

Spatial and non-spatial views preserve the sampled-image owner while making
the requested change explicit.

- `atTime`, `selectChannel`, and `selectNonSpatial` remove a declared
  non-spatial axis.
- `crop`, `flipSpatial`, `permuteSpatial`, and `strideSpatial` describe exact
  integer-affine lattice maps.
- Exact maps can share immutable Ravel storage. A broader transform belongs in
  an explicit materialization or resampling operation.

Here a crop changes the grid shape and its index origin, but it retains the
same physical frame:

```scala mdoc:silent
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray

def checked[A](value: Either[?, A]): A =
  value match
    case Right(result) => result
    case Left(error) => throw new IllegalArgumentException(error.toString)

val frame = checked(Frame.named[D3]("native"))
val grid = checked(Grid.in(frame)(Vector(6, 7, 5), Affine.identity[D3]))
val image =
  checked(
    Image.continuous(
      grid,
      NonSpatialAxes.empty,
      NDArray.tabulate[Double](6, 7, 5)((i, j, k) =>
        100.0 * i + 10.0 * j + k
      )
    )
  )
val crop = checked(image.crop(Vector(1, 2, 1), Vector(3, 4, 2)))

assert(crop.logicalShape == Vector(3, 4, 2))
assert(crop.grid.frame eq image.grid.frame)
assert(crop(0, 0, 0) == image(1, 2, 1))

val rejected = image.crop(Vector(5, 5, 5), Vector(3, 3, 3))
assert(rejected.isLeft)
```

```scala mdoc
(crop.logicalShape, crop(0, 0, 0), rejected.isLeft)
```

The rejected crop is not clamped or silently padded. Its `Left` value carries
the image error, including the axis, origin, requested extent, and source
extent needed to diagnose the request.

## Non-spatial views

A non-spatial selection follows the declared axis order, not a convention
about where time happens to appear in storage:

```scala
image.atTime(2)
image.selectChannel(0)
image.selectNonSpatial(axis = 0, index = 2)
```

The selected value keeps the same spatial grid and metadata. It has one fewer
logical axis, and its Ravel rank is refined when the operation can prove that
the selected axis was dropped.
