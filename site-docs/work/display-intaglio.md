# Display images with Intaglio

The image4s–Intaglio bridge turns sampled values into display objects. It does
not filter scientific values, change the image grid, or resample an image to a
new grid.

```text
image4s operation       changes scientific values or sampled geometry
Intaglio DisplayPlan    changes color, orientation, and pixel placement
reframe4s               computes samples on a different grid
```

## Render a scalar raster

```scala mdoc:silent
import _root_.intaglio.*
import image4s.*
import image4s.geometry.*
import image4s.intaglio.*
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

def checked[A](value: Either[?, A]): A =
  value match
    case Right(result) => result
    case Left(error)   => throw new IllegalArgumentException(error.toString)

val frame = checked(Frame.named[D2]("display-plane"))
val grid = checked(Grid.in(frame)(Vector(3, 2), Affine.identity[D2]))
val space = SampleSpace.create(grid, NonSpatialAxes.empty)
val image =
  checked(
    Sampled.continuous[Double, Rank[2]](
      space,
      NDArray.fromSeq(
        Shape(3, 2),
        Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
      )
    )
  )

val plan =
  DisplayPlan(
    window = DisplayWindow.unsafe(0.0, 5.0),
    orientation = DisplayOrientation(transpose = true, flipX = true),
    interpolation = RasterInterpolation.Nearest
  )

val raster = DisplayBridge.renderRaster(image, plan)

assert(raster.width == 2)
assert(raster.height == 3)
assert(image.logicalShape == Vector(3, 2))
```

Windowing, palette choice, transpose, flips, and the interpolation hint belong
to the `DisplayPlan`. The bridge applies orientation while packing pixels. The
source `Sampled` value and its grid do not change.

## Lower an axis-aligned regular field

```scala mdoc:silent
val field = checked(DisplayBridge.toIntaglioField(image))

assert(field.xAxis.coordinate(0).contains(0.0))
assert(field.yAxis.coordinate(0).contains(0.0))
```

Regular-field lowering retains physical axis coordinates and is available only
for a separable axis-aligned D2 affine. Reflected axes are represented by
reversing display values so Intaglio axes remain increasing.

A rotated or sheared grid is not a regular axis-aligned field:

```scala mdoc:silent
val shearedAffine =
  checked(
    Affine.fromRowMajor[D2](
      Vector(
        1.0, 0.25, 0.0,
        0.0, 1.0, 0.0,
        0.0, 0.0, 1.0
      )
    )
  )
val shearedGrid = checked(Grid.in(frame)(Vector(3, 2), shearedAffine))
val sheared =
  checked(
    Sampled.continuous[Double, Rank[2]](
      SampleSpace.create(shearedGrid, NonSpatialAxes.empty),
      image.data
    )
  )

assert(DisplayBridge.toIntaglioField(sheared).isLeft)
```

Render a raster in source-index coordinates and place it with an Intaglio scene
transform, or resample explicitly with reframe4s. The bridge does not silently
discard off-diagonal affine terms.

## Overlay a mask

```scala mdoc:silent
val mask =
  checked(
    Sampled.mask(
      space,
      NDArray.fromSeq(
        Shape(3, 2),
        Vector(false, true, false, false, true, false)
      )
    )
  )
val overlay =
  MaskOverlay(
    foreground = Rgba32.unsafe(255, 0, 0),
    opacity = DisplayOpacity.Opaque
  )
val overlaid =
  checked(DisplayBridge.renderRasterWithMask(image, mask, plan, overlay))

assert(overlaid.width == raster.width)
assert(overlaid.height == raster.height)
```

The current overlay API checks equal logical shape and uses one shared
sample-to-pixel mapping. It does **not** perform an exact `SampleSpace`
alignment check. Callers combining independently sourced masks should establish
alignment before display rather than treating shape equality as proof.

## Render deterministic label colors

```scala mdoc:silent
val labels =
  checked(
    Sampled.categorical[Int, Rank[2]](
      space,
      NDArray.fromSeq(Shape(3, 2), Vector(0, 7, 7, 11, 7, 11))
    )
  )
val labelRaster = DisplayBridge.renderLabels(labels, LabelPalette())

assert(labelRaster.pixel(1, 0) == labelRaster.pixel(2, 0))
assert(LabelPalette().color(7) == LabelPalette().color(7))
```

Code zero is transparent by default. Each nonzero `Int` code maps to the same
opaque color regardless of traversal order or process lifetime.

## Render an orthogonal D3 slice

```scala mdoc:silent
val frame3 = checked(Frame.named[D3]("display-volume"))
val grid3 = checked(Grid.in(frame3)(Vector(2, 3, 2), Affine.identity[D3]))
val volume =
  checked(
    Sampled.continuous[Double, Rank[3]](
      SampleSpace.create(grid3, NonSpatialAxes.empty),
      NDArray.tabulate[Double](2, 3, 2) { (x, y, z) =>
        x + 2.0 * y + 6.0 * z
      }
    )
  )
val slice =
  checked(
    DisplayBridge.renderSliceRaster(
      volume,
      axis = SliceAxis.Z,
      index = 1,
      plan = DisplayPlan(DisplayWindow.unsafe(0.0, 11.0))
    )
  )

assert(slice.width == 2)
assert(slice.height == 3)
```

Slice axes are source-grid axes. The bridge rejects an out-of-range index and
does not produce oblique slices or infer a spatial resampling transform.

## What changed?

| Display operation | Scientific values | Sample space | Appearance output |
| --- | --- | --- | --- |
| `renderRaster` | Unchanged | Unchanged | Windowed, colored, oriented pixels |
| `toIntaglioField` | Unchanged | Unchanged | Axis-aligned regular field |
| `renderRasterWithMask` | Unchanged | Unchanged | Composited raster |
| `renderLabels` | Unchanged label codes | Unchanged | Deterministic color raster |
| `renderSliceRaster` | Unchanged volume | Unchanged | Orthogonal source-index slice raster |

For the difference between live ownership and exact sampling congruence, read
[Identity, reconstruction, and alignment](../deeper/identity-and-alignment.md).
