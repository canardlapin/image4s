# Filtering

Filtering lives in `image4s-filter`, so it is an optional dependency on top of
`image4s-core`. A filter receives a typed continuous image and returns a typed
result with the sampled space still attached.

Gaussian scale can be expressed in sample units or physical frame units. The
choice is visible at the call site. For a grid with anisotropic spacing,
`SpatialSigma.frame` lets the implementation derive the corresponding sample
scale; use it only when the grid supports the requested separable operation.

This example uses a sample-space Gaussian and the default `Same` extent:

```scala mdoc:silent
import image4s.*
import image4s.filter.gaussianBlur
import image4s.geometry.*
import image4s.ops.SpatialSigma
import ravel.DType.given
import ravel.NDArray

def checked[A](value: Either[?, A]): A =
  value match
    case Right(result) => result
    case Left(error) => throw new IllegalArgumentException(error.toString)

val frame = checked(Frame.named[D2]("plane"))
val grid = checked(Grid.in(frame)(Vector(9, 9), Affine.identity[D2]))
val image =
  checked(
    Image.continuous(
      grid,
      NonSpatialAxes.empty,
      NDArray.tabulate[Double](9, 9) { (row, column) =>
        if row == 4 && column == 4 then 1.0 else 0.0
      }
    )
  )
val sigma = checked(SpatialSigma.samples[D2](1.0))
val blurred = checked(image.gaussianBlur(sigma))

assert(blurred.logicalShape == image.logicalShape)
assert(blurred.grid.sameRuntimeOwnerAs(image.grid))
```

```scala mdoc
(blurred.logicalShape, blurred(4, 4), blurred.data.elementsIterator.sum)
```

The filter returns an `Either[OpError, ...]`. Invalid scales, unsupported
physical assumptions, and unsupported execution plans remain visible rather
than becoming a fallback to a different algorithm.

## Extent and storage choices

The output extent is explicit:

- `FilterExtent.same(border)` keeps the input shape and defines border
  behavior;
- `FilterExtent.valid` removes the neighborhood halo from the output; and
- `FilterExtent.full(border)` expands the output around the input.

For a floating continuous image, `gaussianBlur` preserves the floating
storage. For integer-backed continuous data, use `gaussianBlurTo[Float]` or
`gaussianBlurTo[Double]` and choose the output extent deliberately. The output
type is not inferred by silently truncating filtered values back into integer
storage.
