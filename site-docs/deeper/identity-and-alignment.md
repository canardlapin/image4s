# Identity, reconstruction, and alignment

Most image4s users can construct an image, select views, filter it, and display
it without working directly with registries or alignment evidence. These
mechanisms matter at persistence, cache, plugin, and multi-file boundaries
where independently created objects must be related safely.

Four different questions arise at those boundaries:

1. Do these values use the same live owner?
2. Do these reconstructed objects carry the same persistent identity?
3. Do these sample spaces describe exactly the same sampled locations?
4. Are their geometries merely close within a diagnostic tolerance?

The answers are not interchangeable.

## 1. Same live owner

A live owner is one in-memory `Frame`, `Grid`, or `SampleSpace` object. Reusing
the same `SampleSpace` gives pointwise methods their most precise static proof.

```scala mdoc:silent
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

val liveObjects =
  for
    liveFrame <- Frame.named[D2]("live")
    liveGrid <- Grid.in(liveFrame)(Vector(2, 2), Affine.identity[D2])
  yield (liveFrame, liveGrid)

val (liveFrame, liveGrid) =
  liveObjects.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
val liveSpace = SampleSpace.create(liveGrid, NonSpatialAxes.empty)

assert(liveSpace.sameRuntimeSpaceAs(liveSpace))
```

This relation is strongest inside one running process and disappears when a
record is serialized.

## 2. Same persistent identity

Persistent frames and grids carry validated keys. Independently constructed
objects can have the same key while retaining different live owner tokens.

```scala mdoc:silent
val persistentFrames =
  for
    frameId <- FrameId.parse("scanner-frame")
    leftFrame <- Frame.persistentNamed[D2](
      frameId,
      "scanner",
      LengthUnit.Millimeter,
      CoordinateConvention.RAS
    )
    rightFrame <- Frame.persistentNamed[D2](
      frameId,
      "scanner restored elsewhere",
      LengthUnit.Millimeter,
      CoordinateConvention.RAS
    )
  yield (leftFrame, rightFrame)

val (leftFrame, rightFrame) =
  persistentFrames.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )

assert(!leftFrame.sameRuntimeOwnerAs(rightFrame))
assert(leftFrame.samePersistentKeyAs(rightFrame))
```

Frame presentation metadata such as the label does not change the persistent
key. Spatial rank, length unit, and coordinate convention do. A key collision
with different structural fields is rejected by the registry.

`Frame.Registry` and `Grid.Registry` are immutable. Restoring a validated
record returns both the resolved live object and an updated registry. Repeated
restoration through that updated registry reuses the registered live owner.
Records are neutral persistence values; registries establish process-local
ownership.

## 3. Exact sampling congruence

Persistent identity and sampling congruence answer different questions. Two
grids with different persistent grid IDs can still sample exactly the same
points in aligned frames.

```scala mdoc:silent
val persistentSpaces =
  for
    leftGridId <- GridId.parse("left-grid")
    leftGrid <- Grid.createPersistent(
      leftGridId,
      leftFrame
    )(Vector(2, 2), Affine.identity[D2])
    rightGridId <- GridId.parse("right-grid")
    rightGrid <- Grid.createPersistent(
      rightGridId,
      rightFrame
    )(Vector(2, 2), Affine.identity[D2])
    leftSpace = SampleSpace.create(leftGrid, NonSpatialAxes.empty)
    rightSpace = SampleSpace.create(rightGrid, NonSpatialAxes.empty)
  yield (leftGrid, rightGrid, leftSpace, rightSpace)

val (leftGrid, rightGrid, leftSpace, rightSpace) =
  persistentSpaces.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
val exact =
  leftSpace
    .alignExact(rightSpace)
    .fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )

assert(!leftGrid.samePersistentKeyAs(rightGrid))
assert(exact.left.eq(leftSpace))
assert(exact.right.eq(rightSpace))
```

`alignExact` requires aligned frames, equal spatial shape, identical affine
coefficients, and equal ordered non-spatial axis records. The resulting
`SamplingAlignment` is reusable permission for `zipWithAligned` or `rebind`.

```scala mdoc:silent
val rightImage =
  Image
    .continuous(
      rightSpace,
      NDArray.fromSeq(Shape(2, 2), Vector(1.0, 2.0, 3.0, 4.0))
    )
    .fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )
val rebound = rightImage.rebind(exact.reverse)

assert(rebound.data.eq(rightImage.data))
assert(rebound.sampleSpace.eq(leftSpace))
```

Rebinding changes the live owner recorded by the sampled value after the proof;
it does not copy or resample data.

## 4. Approximate geometric congruence

Approximate congruence answers whether two affines are numerically close within
a caller-provided tolerance. Non-spatial axis records remain exact.

```scala mdoc:silent
val nearResult =
  for
    shiftedAffine <- Affine.fromRowMajor[D2](
      Vector(
        1.0, 0.0, 1.0e-7,
        0.0, 1.0, 0.0,
        0.0, 0.0, 1.0
      )
    )
    nearGrid <- Grid.in(rightFrame)(Vector(2, 2), shiftedAffine)
  yield nearGrid

val nearGrid =
  nearResult.fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
val nearSpace = SampleSpace.create(nearGrid, NonSpatialAxes.empty)

assert(leftSpace.alignExact(nearSpace).isLeft)
assert(leftSpace.approximatelyCongruentTo(nearSpace, 1.0e-6).isRight)
```

The approximate result is diagnostic evidence. It is intentionally not a
`SamplingAlignment`, so it cannot authorize pointwise arithmetic. Decide
whether to reject the mismatch or resample onto a chosen grid with reframe4s.

## Choose the relation required by the task

| Relation | Typical use | Permits pointwise image arithmetic? |
| --- | --- | --- |
| Same live owner | Ordinary in-process pipelines | Yes, through shared-owner APIs |
| Same persistent identity | Restoration, caches, and provenance | Not by itself |
| Exact sampling congruence | Independently constructed but exactly aligned inputs | Yes, through `SamplingAlignment` |
| Approximate congruence | Diagnostics and resampling decisions | No |

Return to [Combine aligned images](../work/combine-aligned-images.md) for the
ordinary arithmetic workflow, or consult
[Errors and checked boundaries](../reference/errors-and-checked-boundaries.md)
when a reconstruction or alignment check fails.
