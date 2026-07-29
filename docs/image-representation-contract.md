# Canonical image representation contract

This document is the normative contract for Mote epic
`bd-01KYNR748JZYEP3A5Q1H9ZB3AR`. It refines `DEC-011`, `DEC-015`, and
`API-002` in `PRD.json`. The executable court lives in
`ImageRepresentationContractSuite` and
`ImageRepresentationPerformanceSuite`.

## One hierarchy, three responsibilities

| Layer | Authority | May not own |
|---|---|---|
| Ravel `NDArray[A,R]` | dense storage, rank, shape, strides, views, layout, elementwise and reduction kernels | image geometry, axis meaning, neuroimaging policy |
| image4s `Sampled[F,D,A,Role,R]` | one immutable Ravel value plus `Grid`, ordered non-spatial axes, and field role | a second buffer, stride model, flat-index convention, or kernel stack |
| ScalaFIM compatibility surface | checked legacy ingress, source-facing aliases or extensions, neuroimaging workflows and policy | a private dense array hierarchy or a second sampled-image owner |

`Image`, `ImageSeries`, `ScalarImage`, `ComponentImage`, and `LabelImage` are
aliases or zero-copy semantic views of `Sampled`. Dense `NeuroVol` and
`NeuroVec` must converge on the same value. `SparseNeuroVec` is not a dense
image: its target representation is typed spatial support plus compact rank-2
Ravel storage.

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

Non-spatial axes retain their declared order and identity. Time, channel,
echo, coil, direction, and batch axes never participate in spatial maps.

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

- `Sampled.create` and its role-specific constructors retain an immutable
  `NDArray` by reference. They do not copy it.
- Immutable Ravel views are valid zero-copy `Sampled` storage, including
  reversed and strided layouts.
- Mutable and borrowed inputs cross the current public ownership boundary only
  through an explicit copy.
- Fixing a non-spatial coordinate, such as selecting time `t`, preserves the
  same spatial `Grid`, removes that non-spatial axis, and should be a zero-copy
  Ravel view even when it is not contiguous. `selectNonSpatial`, `selectAxis`,
  `selectTime`, `selectChannel`, and `selectDirection` provide these checked
  views.
- `spatialView(origin, shape)` returns a zero-copy crop. It creates a new grid
  in the same frame and shifts the complete affine so new index zero maps to
  the selected source origin.
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
