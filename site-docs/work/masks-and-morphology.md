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

val setup =
  for
    frame <- Frame.named[D2]("mask-plane")
    grid <- Grid.in(frame)(Vector(7, 7), Affine.identity[D2])
    time <- Axis.regular(
      "time",
      AxisKind.Time,
      extent = 2,
      origin = 0.0,
      step = 1.0,
      unit = AxisUnit.Seconds
    )
    axes <- NonSpatialAxes.from(Vector(time))
    image <- Image.continuous(
      grid,
      axes,
      NDArray.tabulate[Float](7, 7, 2) { (x, y, t) =>
        if x == 3 && y == 3 then 4.0f + t else 0.0f
      }
    )
    mask <- image.threshold(2.0f)
  yield (grid, image, mask)

val (grid, image, mask) =
  setup.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

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
val morphology =
  for
    oneSample <- Radius.samples(1)
    box = StructuringElement.box[D2](oneSample)
    cross = StructuringElement.cross[D2](oneSample)
    disk = StructuringElement.disk[D2](oneSample)
    dilated <- mask.dilate(box)
    eroded <- dilated.erode(cross)
    opened <- mask.open(disk)
    closed <- mask.close(disk)
  yield (disk, dilated, eroded, opened, closed)

val (disk, dilated, eroded, opened, closed) =
  morphology.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

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
val physicalDilation =
  (for
    twoMillimetres <- Radius.frame(
      2.0,
      unit = Some(LengthUnit.Millimeter)
    )
    physicalDisk = StructuringElement.disk[D2](twoMillimetres)
    result <- mask.dilate(physicalDisk)
  yield result).fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

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
val preparedRuns =
  for
    openPlan <- BinaryMorphology.prepareOpen(mask, disk)
    firstOpen <- openPlan.run(mask)
    secondOpen <- openPlan.run(mask)
  yield (openPlan, firstOpen, secondOpen)

val (openPlan, firstOpen, secondOpen) =
  preparedRuns.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

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
