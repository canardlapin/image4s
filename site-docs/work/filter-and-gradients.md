# Filter images and compute gradients

Filtering computes new values from spatial neighborhoods. image4s requires the
caller to choose the scale, output extent, output dtype when promotion is
needed, and coordinate domain for gradients. The resulting image keeps a grid
that describes the samples actually produced.

## Build a small continuous image

```scala mdoc:silent
import image4s.*
import image4s.filter.*
import image4s.geometry.*
import image4s.ops.*
import ravel.DType.given
import ravel.NDArray

val setup =
  for
    frame <- Frame.named[D2]("filter-plane")
    grid <- Grid.in(frame)(Vector(9, 9), Affine.identity[D2])
    impulse <- Image.continuous(
      grid,
      NonSpatialAxes.empty,
      NDArray.tabulate[Double](9, 9) { (x, y) =>
        if x == 4 && y == 4 then 1.0 else 0.0
      }
    )
  yield (grid, impulse)

val (grid, impulse) =
  setup.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
```

## Choose the scale's coordinate system

Sample-space sigma measures distance in grid-index steps. Frame-space sigma
measures distance in the frame's length unit and is converted through the grid
affine.

```scala mdoc:silent
val scaledBlurs =
  for
    oneSample <- SpatialSigma.samples[D2](1.0)
    twoMillimetres <- SpatialSigma.frame[D2](
      2.0,
      unit = Some(LengthUnit.Millimeter)
    )
    sampleBlur <- impulse.gaussianBlur(oneSample)
    frameBlur <- impulse.gaussianBlur(twoMillimetres)
  yield (oneSample, sampleBlur, frameBlur)

val (oneSample, sampleBlur, frameBlur) =
  scaledBlurs.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(sampleBlur.logicalShape == impulse.logicalShape)
assert(frameBlur.logicalShape == impulse.logicalShape)
```

An isotropic frame sigma can remain separable on a rotated grid. An anisotropic
frame sigma requires grid axes that map separately to frame axes; otherwise the
current Gaussian implementation returns `OpError.InvalidScale` instead of
approximating a non-separable kernel.

## Choose the output extent

```scala mdoc:silent
val extents =
  for
    same <- impulse.gaussianBlur(
      oneSample,
      extent = FilterExtent.same(Border.reflect)
    )
    valid <- impulse.gaussianBlur(
      oneSample,
      extent = FilterExtent.valid
    )
    full <- impulse.gaussianBlur(
      oneSample,
      extent = FilterExtent.full(Border.Constant(0.0))
    )
  yield (same, valid, full)

val (same, valid, full) =
  extents.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(same.logicalShape == Vector(9, 9))
assert(valid.logicalShape == Vector(3, 3))
assert(full.logicalShape == Vector(15, 15))
```

- `Same` keeps the input spatial shape and uses the chosen border rule.
- `Valid` emits only centers whose complete neighborhood lies in the input.
- `Full` includes every overlap between the kernel and input and requires a
  border rule.

`Valid` and `Full` derive new grid origins as well as new shapes. Reading the
output array without its grid would lose that spatial shift.

## Promote integer-backed values explicitly

Gaussian results are generally fractional. An integer-backed continuous image
therefore uses `gaussianBlurTo[Float]` or `gaussianBlurTo[Double]`.

```scala mdoc:silent
val promotedResult =
  for
    integerImage <- Image.continuous(
      grid,
      NonSpatialAxes.empty,
      NDArray.tabulate[Int](9, 9)((x, y) => x + y)
    )
    promoted <- integerImage.gaussianBlurTo[Double](oneSample)
  yield (integerImage, promoted)

val (integerImage, promoted) =
  promotedResult.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(promoted.logicalShape == integerImage.logicalShape)
```

There is no preserving `gaussianBlur` overload for integer storage. The API
does not silently truncate computed values back to integers.

## State the gradient coordinate domain

```scala mdoc:silent
val gradients =
  for
    indexGradient <- sampleBlur.gradient(IndexCoordinates)
    frameGradient <- sampleBlur.gradient(FrameCoordinates)
  yield (indexGradient, frameGradient)

val (indexGradient, frameGradient) =
  gradients.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(indexGradient.components.size == 2)
assert(frameGradient.components.size == 2)
assert(indexGradient.domain == IndexCoordinates)
assert(frameGradient.domain == FrameCoordinates)
```

Index-coordinate components follow grid-axis order. Frame-coordinate gradients
apply the full affine inverse-transpose to the index gradient. On rotated grids
this mixes components; it is not equivalent to dividing each component by a
nominal spacing.

`gradientTo[Float]` and `gradientTo[Double]` provide the corresponding explicit
promotion for integer-backed continuous images.

## Prepare repeated execution

```scala mdoc:silent
val plannedResult =
  for
    plan <- Gaussian.prepare(
      impulse,
      oneSample,
      policy = ExecutionPolicy(method = FilterMethod.Separable)
    )
    planned <- plan.run(impulse)
  yield (plan, planned)

val (plan, planned) =
  plannedResult.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(planned.logicalShape == impulse.logicalShape)
assert(plan.report.method == SelectedMethod.Separable)

val unsupportedFft =
  Gaussian.prepare(
    impulse,
    oneSample,
    policy = ExecutionPolicy(method = FilterMethod.Fft)
  )
assert(unsupportedFft.isLeft)
```

A prepared plan fixes validation and scheduling choices and reuses its prepared
execution workspace for compatible sequential calls with the same live sample
space. `Direct` and supported `Separable` plans compute the same operation.
Separable optimized execution is currently limited to supported `Same`-extent
operations. Requesting FFT returns an explicit unsupported error; it does not
fall back silently.

## What changed?

| Operation | Values | Spatial grid | Non-spatial axes | Semantic role |
| --- | --- | --- | --- | --- |
| Gaussian `Same` | Newly computed | Same live grid | Preserved as independent batches | Continuous |
| Gaussian `Valid` | Newly computed | Contracted shape and shifted origin | Preserved as independent batches | Continuous |
| Gaussian `Full` | Newly computed | Expanded shape and shifted origin | Preserved as independent batches | Continuous |
| `gaussianBlurTo[B]` | Computed and converted to `B` | Determined by extent | Preserved | Continuous |
| Index gradient | One computed component per grid axis | Same live grid | Preserved | Continuous components |
| Frame gradient | Components transformed by affine inverse-transpose | Same live grid | Preserved | Continuous frame-vector components |

Continue to [Build masks and apply morphology](masks-and-morphology.md) to turn
a continuous result into an explicitly typed mask.
