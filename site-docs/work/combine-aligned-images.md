# Combine aligned images

Pointwise arithmetic is safe when both inputs sample the same spatial points
and carry the same ordered non-spatial coordinates. image4s offers a concise
operation for images with one exact live `SampleSpace` owner and an explicit
proof for independently constructed owners.

## Use the shared-owner operation first

```scala mdoc:silent
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

val gridResult =
  for
    frame <- Frame.named[D2]("pointwise")
    grid <- Grid.in(frame)(Vector(2, 2), Affine.identity[D2])
  yield grid

val grid =
  gridResult.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
val leftSpace = SampleSpace.create(grid, NonSpatialAxes.empty)

val sharedInputs =
  for
    left <- Image.continuous(
      leftSpace,
      NDArray.fromSeq(Shape(2, 2), Vector(1.0, 2.0, 3.0, 4.0))
    )
    right <- Image.continuous(
      leftSpace,
      NDArray.fromSeq(Shape(2, 2), Vector.fill(4)(10.0))
    )
  yield (left, right)

val (left, right) =
  sharedInputs.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

val sum = left.zipWith(right)(_ + _)

assert(sum(1, 1) == 14.0)
assert(sum.sampleSpace.eq(leftSpace))
```

The shared singleton type of `leftSpace` is the proof. `zipWith` computes new
values, retains metadata from the left input, and keeps the left sample-space
owner.

## Align independently constructed sample spaces once

Two `SampleSpace.create` calls produce different live owners even when they
refer to the same grid and ordered axes. Check exact congruence once and reuse
the resulting `SamplingAlignment`.

```scala mdoc:silent
val reconstructedSpace =
  SampleSpace.create(grid, NonSpatialAxes.empty)
val alignedInputs =
  for
    reconstructed <- Image.continuous(
      reconstructedSpace,
      NDArray.fromSeq(Shape(2, 2), Vector.fill(4)(20.0))
    )
    alignment <- left.sampleSpace.alignExact(reconstructedSpace)
  yield (reconstructed, alignment)

val (reconstructed, alignment) =
  alignedInputs.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
val checkedSum =
  left.zipWithAligned(reconstructed, alignment)(_ + _)

assert(checkedSum(1, 1) == 24.0)
assert(checkedSum.sampleSpace.eq(leftSpace))
```

`alignExact` checks exact grid congruence and equality of the ordered axis
records. Shape alone is not enough: a shifted affine, a different frame, a
different axis unit, or different declared coordinates rejects the proof.

## Rebind when several operations share the proof

`rebind` changes the live sample-space owner after alignment without copying
the Ravel data. This is useful when many later operations expect the left
owner.

```scala mdoc:silent
val rebound = reconstructed.rebind(alignment.reverse)
val reboundSum = left.zipWith(rebound)(_ + _)

assert(rebound.data.eq(reconstructed.data))
assert(reboundSum(0, 0) == 21.0)
```

Approximate geometric congruence is diagnostic evidence. It does not produce a
`SamplingAlignment` and therefore does not authorize pointwise arithmetic.

## What changed?

| Step | Values | Spatial grid | Non-spatial axes | Semantic role |
| --- | --- | --- | --- | --- |
| `alignExact` | Unchanged | Compared exactly | Records compared in order | Unchanged |
| `rebind` | Same array object | Rebound to proved owner | Rebound to proved owner | Preserved |
| `zipWith` / `zipWithAligned` | Newly computed | Left input's grid | Left input's axes | Preserved for this overload |

For registry restoration and persistent identity, continue to
[Identity, reconstruction, and alignment](../deeper/identity-and-alignment.md).
