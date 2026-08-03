# Build masks and apply morphology

A mask records Boolean membership over a complete sample space. Thresholding
changes a continuous image into that semantic role explicitly; morphology then
changes mask membership through a spatial neighborhood without treating time
or channel samples as neighbors.

## Threshold a continuous image

```scala mdoc:silent
import image4s.*
import image4s.geometry.*
import image4s.morphology.*
import image4s.ops.*
import ravel.DType.given
import ravel.NDArray

def checked[A](value: Either[?, A]): A =
  value match
    case Right(result) => result
    case Left(error)   => throw new IllegalArgumentException(error.toString)

val frame = checked(Frame.named[D2]("mask-plane"))
val grid = checked(Grid.in(frame)(Vector(7, 7), Affine.identity[D2]))
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
val axes = checked(NonSpatialAxes.from(Vector(time)))
val image =
  checked(
    Image.continuous(
      grid,
      axes,
      NDArray.tabulate[Float](7, 7, 2) { (x, y, t) =>
        if x == 3 && y == 3 then 4.0f + t else 0.0f
      }
    )
  )

val mask = checked(image.threshold(2.0f))

assert(mask.logicalShape == image.logicalShape)
assert(mask.grid.sameRuntimeOwnerAs(image.grid))
assert(mask(3, 3, 0))
assert(!mask(2, 3, 0))
```

`MaskImage` is not a numeric categorical image. It supports membership and
binary morphology rather than interpolation or label arithmetic. Thresholding
preserves the sample space and metadata while changing the element type and
semantic role.

## Choose a radius and structuring element

```scala mdoc:silent
val oneSample = checked(Radius.samples(1))
val box = StructuringElement.box[D2](oneSample)
val cross = StructuringElement.cross[D2](oneSample)
val disk = StructuringElement.disk[D2](oneSample)

val dilated = checked(mask.dilate(box))
val eroded = checked(dilated.erode(cross))
val opened = checked(mask.open(disk))
val closed = checked(mask.close(disk))

assert(dilated.logicalShape == mask.logicalShape)
assert(eroded.logicalShape == mask.logicalShape)
assert(opened.logicalShape == mask.logicalShape)
assert(closed.logicalShape == mask.logicalShape)
```

The available flat elements are:

| Element | Spatial support |
| --- | --- |
| Box | Every offset within the radius on every axis |
| Cross | Offsets along one grid axis at a time |
| Disk | Euclidean support for D2 |
| Ball | Euclidean support for D3 |

Disk preparation rejects dimensions other than D2. Ball preparation rejects
dimensions other than D3. `Radius.samples` rejects negative integers before an
element is used.

## Express radius in frame units

```scala mdoc:silent
val twoMillimetres =
  checked(
    Radius.frame(
      2.0,
      unit = Some(LengthUnit.Millimeter)
    )
  )
val physicalDisk = StructuringElement.disk[D2](twoMillimetres)
val physicalDilation = checked(mask.dilate(physicalDisk))

assert(physicalDilation.logicalShape == mask.logicalShape)
assert(Radius.samples(-1).isLeft)
```

A frame-radius disk or ball is lowered against the input grid during
preparation. image4s uses the affine-induced physical metric, including axis
spacing and cross-axis terms, rather than assuming isotropic samples. A
degenerate spatial affine reports an invalid-scale error.

## Non-spatial axes are independent batches

The structuring element above has two spatial coordinates because the grid is
D2. The time axis is retained but never added to the neighborhood. Each time
point is thresholded and processed independently.

## Prepare a sequential multi-pass operation

Opening and closing each require two passes. A prepared morphology object
lowers the element once and reuses internal workspace for repeated sequential
calls with the same live sample-space owner.

```scala mdoc:silent
val openPlan = checked(BinaryMorphology.prepareOpen(mask, disk))
val firstOpen = checked(openPlan.run(mask))
val secondOpen = checked(openPlan.run(mask))

assert(firstOpen.sameValuesAs(secondOpen)(_ == _))
assert(openPlan.support.offsets.nonEmpty)
```

The prepared object is stateful workspace for sequential use. Do not share one
instance across concurrent executions.

## What changed?

| Operation | Values | Spatial grid | Non-spatial axes | Semantic role |
| --- | --- | --- | --- | --- |
| `threshold` | Newly computed Booleans | Same live grid | Preserved | Continuous → Mask |
| `dilate` | Newly computed membership | Same live grid | Preserved as independent batches | Mask |
| `erode` | Newly computed membership | Same live grid | Preserved as independent batches | Mask |
| `open` | Erosion then dilation | Same live grid | Preserved as independent batches | Mask |
| `close` | Dilation then erosion | Same live grid | Preserved as independent batches | Mask |

Continue to [Display images with Intaglio](display-intaglio.md) to overlay the
mask without changing the scientific image.
