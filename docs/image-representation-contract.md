# Canonical image representation contract

This document is the normative contract for Mote epic
`bd-01KYNR748JZYEP3A5Q1H9ZB3AR`. It refines `DEC-011`, `DEC-015`, and
`API-002` in `PRD.json`. The executable court lives in
`ImageRepresentationContractSuite` and
`ImageRepresentationPerformanceSuite`. Sampling and identity hardening is
tracked by `bd-01KYRBYM4C286QC9CQ0WCA03NB`.

## One hierarchy, three responsibilities

| Layer | Authority | May not own |
|---|---|---|
| Ravel `NDArray[A,R]` | dense storage, rank, shape, strides, views, layout, elementwise and reduction kernels | image geometry, axis meaning, neuroimaging policy |
| image4s `Sampled[S,A,Sem,R]` | one immutable Ravel value plus its complete `SampleSpace` owner and proven value semantics | a second buffer, stride model, flat-index convention, or kernel stack |
| ScalaFIM compatibility surface | checked legacy ingress, source-facing aliases or extensions, neuroimaging workflows and policy | a private dense array hierarchy or a second sampled-image owner |

`Image`, `ContinuousImage`, `CategoricalImage`, and `MaskImage` are aliases of
`Sampled`. `TimeSeriesView` and `ComponentAxisView` are checked zero-copy
wrappers that retain the original `Sampled`; they are not containers. Dense
`NeuroVol` and `NeuroVec` must converge on the same value. `SparseNeuroVec` is
not a dense image: its target representation is typed spatial support plus
compact rank-2 Ravel storage.

## Value semantics and semantic views

The semantic parameter is unbounded so downstream libraries can define their
own tags. Construction requires `ValueSemantics[A,Sem]`; a caller cannot attach
an arbitrary tag to `A` without supplying that evidence. The standard tags are
`Continuous`, `Categorical`, and `Mask`.

- Standard continuous construction requires `LinearInterpolable[A]`. It is not
  synonymous with `Double`; `DoubleContinuousImage` / `FloatContinuousImage`
  exist only as narrow conveniences.
- Standard categorical construction requires `IntegralDType[A]` (Byte, Short,
  Int, Long). Floating and Boolean categorical images are rejected at the
  standard factory. A Boolean mask is not a categorical label image.
- `MaskImage` is Boolean logical support (`Sampled[..., Boolean, Mask, ...]`).
- `Sampled.dtype` forwards Ravel's `DType[A]`; image4s does not invent a
  parallel dtype hierarchy.
- Linear reference sampling requires `LinearSampling[A,Sem]`. Categorical and
  mask values do not receive that capability by default.
- Nearest-neighbour sampling returns `A` unchanged.
- Scalar, component, time-series, vector, and tensor structure is not inferred
  from a semantic tag.

`TimeSeriesView.from` proves that exactly one `Time` axis exists.
`ComponentAxisView.from` proves that exactly one caller-selected axis kind
exists, while making no claim about physical-frame transformation laws. Missing
and ambiguous axes are typed `ImageError` values. Both wrappers retain the
original image and data by reference.

`mapValues` and `zipWith` preserve the existing element type and proven
semantic tag. `mapValuesAs`, `zipWithAs`, and `zipWithAlignedAs` require
explicit target `ValueSemantics` evidence when the output type or semantic tag
changes. They never silently guess a new output semantic tag. The
shape-preserving kernels are owned by Ravel.

## Logical coordinates

Spatial indices always precede non-spatial indices:

```text
D2 image:         (i, j)
D3 image:         (i, j, k)
D3 time series:   (i, j, k, t)
D3 time+channel:  (i, j, k, t, c)
```

The corresponding shape is always:

```text
grid.shape ++ nonSpatialAxes.shape
```

For a D3 grid, `i`, `j`, and `k` are grid axes 0, 1, and 2. The common names
`x`, `y`, and `z` are shorthand for those grid axes only. In particular, `k`
or `z` selects a slice along the third grid axis. It does not assert that the
axis points along anatomical or world Z. `Grid.indexToFrame` is the sole
authority for physical orientation, permutation, reflection, spacing, and
origin.

Non-spatial axes retain their declared order and coordinate sampling. Each axis
is ordinal, regular numeric, explicit numeric, or categorical. Numeric
coordinates retain their unit identifier. Time, channel, echo, coil,
direction, and batch axes never participate in spatial maps. Domain metadata
such as BIDS entities, slice-timing policy, phase encoding, and gradient tables
does not become part of the core axis record.

## Geometry identity

Live owner identity, persistent-key identity, exact geometric congruence, and
approximate congruence are separate relations.

- `Frame.named`, `Frame.ephemeral`, `Grid.in`, and `Grid.forFrame` create
  ephemeral owners without generating an ID.
- Persistent construction requires caller-supplied `FrameId` and `GridId`
  values. Pure constructors never generate random persistent-looking IDs.
- `FrameKey` contains ID, rank, length unit, and coordinate convention.
  `FrameMetadata` contains the renameable display label.
- `GridKey` contains ID, the complete frame key, shape, and canonical affine
  values. Affine validation tolerance and diagnostics are construction policy,
  not persistent identity.
- Frame and grid registries are immutable. Registration and restoration return
  an updated registry, so an earlier value remains safe to share concurrently.
- Restoration through the same registry recovers its registered live owner.
  Separate registries may reconstruct distinct live owners with the same key;
  explicit alignment transports values between them.
- Exact grid congruence may hold for distinct grid keys. Approximate congruence
  always requires a caller-supplied finite, non-negative tolerance.

## Sampling identity and comparison

`SampleSpace` and `Sampled` use reference identity for Scala `equals` and
`hashCode`. Hashing a large image is therefore constant-time and never scans
its values. Descriptive `ImageMetadata` does not participate in any sampling
relation.

The public relations answer separate questions:

- `sameRuntimeSpaceAs` tests one exact live `SampleSpace` owner.
- `persistentRelationTo` and `samePersistentSpaceAs` compare persistent grid
  keys plus ordered coordinate-bearing axis records; ephemeral inputs are
  reported explicitly.
- `alignExact` proves exact grid congruence and exact ordered axis records once,
  returning reusable `SamplingAlignment[L,R]`.
- `approximatelyCongruentTo` applies a finite non-negative tolerance only to
  grid geometry. Non-spatial axis coordinates remain exact.
- `sameValuesAs` is an extensional value comparison using caller-supplied
  element equality; it does not imply sampling compatibility.
- laws-level `allClose` and `allCloseAligned` require a validated
  `NumericTolerance` and define NaN behavior explicitly.

Axis units are identifiers, not an implicit units-of-measure conversion
system. Thus a regular axis in seconds does not exactly align with an
equivalent record in milliseconds. A caller must explicitly re-express both
axes in one common unit before seeking exact alignment.

`rebind` consumes exact alignment evidence, changes only the live space
reference and static owner, and retains the immutable Ravel array and metadata
by reference. `sharesStorageWith` reports `SameArrayObject` only when image4s
can prove the exact same public Ravel array object. Ravel does not currently
expose a general storage-alias proof for separate view objects, so all other
cases are deliberately `Unknown`.

## Dimensional geometry scope

`Affine[D]` (also named `AffineIso[D]`) and `Grid[F,D]` deliberately map a
D-dimensional lattice isomorphically into a D-dimensional frame. The core
supports D2-in-D2 and D3-in-D3. It does not treat a native D2 plane as if it
were intrinsically D3 merely because its patient frame is D3.

Today, callers that require a D2 acquisition in D3 may retain it as a D3 grid
with singleton third extent or keep its embedded-plane geometry downstream.
A future core generalization must introduce separately named
`AffineEmbedding[I,W]` and `EmbeddedGrid[F,I,W]` types with projection residual
and off-plane failure. It must not change the established meaning of
`Affine[D]`, `Grid[F,D]`, or `Point[F,D]`. `Sampled[S,...]` inherits this
scope from the grid inside `S`.

`Affine.fromOriginSpacingDirection` requires orthonormal direction cosines,
including reflections, and keeps positive anisotropic spacing separate.
General invertible bases use `Affine.fromRowMajor`. Every affine records an
inverse residual and an infinity-norm condition estimate for its linear basis,
and rejects caller-set diagnostic limits that it exceeds. Translation does not
artificially inflate the basis condition estimate.

`LatticeIndex[D]` is an unbounded integer coordinate.
`ContinuousIndex[D]` is likewise unbounded. `GridIndex[G,D]` is checked against
every finite extent and owned by one exact live grid; it cannot be passed to a
different grid. The old rank-only `Index` name is a migration alias only.

## Rank, shape, and failure behavior

- The Ravel rank equals the number of spatial axes plus the number of
  non-spatial axes.
- Every extent is positive at the image boundary.
- Construction requires exact shape equality; broadcasting is never implicit.
- Dynamic checked lookup reports spatial and non-spatial rank and bounds
  failures as `ImageError`. Call `valueAt(spatial, nonSpatial)` when indices
  arrive as runtime vectors.
- A statically ranked image supports direct logical `apply`: `image(i,j)`,
  `image(i,j,k)`, or `image(i,j,k,t)`. These methods delegate to Ravel without
  constructing index collections. Bounds and arity failures use Ravel's ranked
  indexing errors.
- Call `requireDataRank[N]` to refine a runtime-known rank without copying
  storage before using ranked access or a second rank-dropping view.
- Shape, affine, axis, and ownership validation happens once at construction or
  at an explicit compatibility boundary, not per element in a hot loop.

## Physical layout is not image meaning

The pinned Ravel revision
`f804ba51242aae3a1442b3855a20bd896ffa8b64` creates canonical C-order arrays:
the last logical axis is physically fastest. Ravel views may instead have
positive, negative, broadcast, or permuted strides. All such layouts remain
valid image storage when their logical shape matches the image axes.

image4s therefore has no public flat-index contract. Code may use ranked
coordinates, Ravel logical iteration, or a Ravel kernel. It may not derive a
physical address from `grid.shape`, assume contiguity, or expose raw offsets as
image semantics.

Live ScalaFIM revision `6b5d4993d9acc59873e00eed5211b56728968a78`
uses first-axis-fastest storage. For a legacy D3 volume:

```text
legacy3(i,j,k) = i + nx * (j + ny * k)
```

and for a D3 time series:

```text
legacy4(i,j,k,t) = i + nx * (j + ny * (k + nz * t))
```

Passing that flat buffer directly to canonical Ravel storage of shape
`(nx,ny,nz[,nt])` changes logical values. A compatibility ingress must validate
rank, extents, size overflow, exact buffer length, geometry, axis metadata, and
ownership, then perform one of two explicit actions:

1. construct a proven zero-copy Ravel view when a safe ownership-preserving
   public Ravel boundary can represent the source storage and strides; or
2. materialize one canonical logical-order copy and report that materialization
   in its API and performance evidence.

There is no silent transpose, implicit reshape, or wrapper that keeps the
legacy flat-order API alive inside image4s.

## Ownership, views, and slicing

- `Sampled.create` and its semantic constructors retain an immutable
  `NDArray` by reference. They do not copy it.
- Immutable Ravel views are valid zero-copy `Sampled` storage, including
  reversed and strided layouts.
- Mutable and borrowed inputs cross the current public ownership boundary only
  through an explicit copy.
- Fixing a non-spatial coordinate, such as selecting time `t`, preserves the
  same spatial `Grid`, removes that non-spatial axis, and should be a zero-copy
  Ravel view even when it is not contiguous. `selectNonSpatial`, `selectAxis`,
  `selectTime`, `selectChannel`, and `selectDirection` provide these checked
  views. `selectNonSpatialWithCoordinate` additionally returns the declared
  coordinate that was fixed.
- `LatticeMap[D]` represents exactly the target-to-source integer-affine maps
  that Ravel can execute as signed slicing plus axis permutation. Identity,
  crop, flip, permutation, stride, and general strided permutation are checked
  constructors. `followedBy` composes maps in pullback order.
- `Sampled.view(map)` composes the source grid affine with the discrete map and
  applies the corresponding zero-copy Ravel view. `spatialView`,
  `flipSpatial`, `permuteSpatial`, and `strideSpatial` are approachable
  facades over that one operation.
- Arbitrary or interpolating maps are not represented as exact views. They
  require an explicitly materialized result or reframe4s resampling.
- `permuteNonSpatial` reorders coordinate-bearing axes and their Ravel axes
  together. `replaceAfterNonSpatialReduction` removes one declared axis only
  after validating the caller-supplied Ravel reduction result.
- Selecting or dropping a spatial axis is a geometry operation, not merely an
  array slice. It must construct the correctly transformed lower-dimensional
  grid or retain an explicitly typed slice geometry.
- `canonicalLayout` returns the same `Sampled` object when its Ravel layout is
  already canonical and otherwise copies logical values into canonical C
  order. `materializedCopy` always allocates a new canonical buffer.

For example, a statically ranked D3 time series can select a volume without
copying its voxel values:

```scala
val sample = series(i, j, k, t)
val volume = series.selectTime(t).toOption.get
val sameSample = volume(i, j, k)
```

The selected `volume` has the same `Grid`, one fewer non-spatial axis, and a
rank-3 Ravel view backed by the series storage.

## Executable evidence court

The shared court runs on JVM and Scala.js and covers:

- randomized D3-plus-Time differential checks against the independent
  first-axis-fastest formula above;
- a regression proving that naive flat-buffer adoption changes values;
- deterministic logical and weighted checksums;
- strided and reversed Ravel views;
- separation of third-grid-axis slicing from world-axis orientation;
- exact spatial/non-spatial shape and typed-error behavior already enforced by
  the image4s suites.

The JVM court runs the same coordinate workloads in C-order traversal,
legacy-volume traversal, z-slice traversal, and time-series traversal. It
records median time, per-thread allocation, logical checksum, and weighted
checksum for direct Ravel access and checked `Sampled.valueAt` access. These
numbers are development baselines, not cross-runtime release claims. Visible
losses remain recorded until the zero-overhead image4s access bead
`bd-01KYNR806KDX4GFH3B486AM1CG` replaces them with a matched rerun.
