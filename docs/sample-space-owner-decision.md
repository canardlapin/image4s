# Sample-space owner decision

Status: accepted for implementation  
Mote: `bd-01KYRBYMVEEQJSH1ZZ089FTBGX`

## Decision

Make the complete live sample-space value the principal static owner:

```scala
final class Sampled[
    S <: SampleSpace,
    A,
    Sem,
    R <: AnyRank
] private (
    val sampleSpace: S,
    val data: NDArray[A, R],
    val metadata: ImageMetadata,
    ...
)
```

`SampleSpace` carries its spatial dimension and frame owner as type members.
The owner parameter `S` is the exact singleton type of the complete space
value. It is phantom at runtime.

Construction from an explicit space preserves that exact owner:

```scala
def continuous[A, R <: AnyRank](
    space: SampleSpace
)(
    data: NDArray[A, R]
)(using ValueSemantics[A, Continuous]): Either[
  ImageError,
  Sampled[space.type, A, Continuous, R]
]
```

The direct grid-and-axes constructor remains available and returns an image
with a hidden captured space owner. Users only name a space when they want
multiple independently constructed images to be statically pointwise
compatible:

```scala
val axes  = NonSpatialAxes.from(Vector(time)).toOption.get
val space = SampleSpace.create(grid, axes)
val left  = Image.continuous(space, leftData).toOption.get
val right = Image.continuous(space, rightData).toOption.get
val sum   = left.zipWith(right)(_ + _)
```

Ordinary derived values retain or hide owners according to semantics:

- value-only operations and materialized copies retain `S`;
- selecting an axis, crop, flip, permutation, and stride create a fresh hidden
  sample-space owner;
- `SamplingAlignment[left.type,right.type]` is checked once;
- `rebind` changes only the static owner and sample-space reference, retaining
  immutable Ravel data by reference.

Same-owner `zipWith` is total with respect to sampling compatibility. Its
output element type, semantic tag, Ravel dtype, and rank remain explicit.

## Why this owner earns its cost

The previous `Sampled[F,D,A,Sem,R]` proved only a common physical frame. Two
different grids and non-spatial schedules in that frame have the same static
type, so every pointwise operation must repeat a runtime sampling check.

With `Sampled[S,A,Sem,R]`, the compiler rejects distinct space owners before a
pointwise operation is called. Checked alignment returns reusable evidence and
zero-copy rebinding makes subsequent operations total. The owner therefore
represents the exact invariant that pointwise field algebra needs.

The production implementation verifies:

- common D3 construction with a regular Time axis;
- total same-owner `zipWith`;
- rejection of a generic distinct-owner combination;
- fresh captured owners for selection, crop, flip, permutation, and stride;
- checked alignment and data-sharing rebind;
- `SomeSampled`-style dynamic packaging of owner, dimension, and rank;
- direct grid construction with no user-visible proof object;
- downstream functions over wildcard-captured images without an explicit
  owner type parameter.

The temporary test-only prototype was removed after the production migration
landed. `SamplingAlignmentSuite`, `SampledSuite`, and
`ImageRepresentationPerformanceSuite` now exercise the real public
representation, so there is no parallel field hierarchy even in the test
surface.

## Compiler and representation evidence

The shared production court passes on the JVM and Scala.js. Compile-negative
fixtures reject combining `Sampled[L,...]` and `Sampled[R,...]` without
alignment.
Positive fixtures show that two values constructed from the same stable space
have a common exact owner and combine without a compatibility branch.

The matched JVM court traverses 22,440 values for direct Ravel and ranked
owner-parameter `Sampled`. Both record 40 bytes for the complete measured
traversal. Timing from a non-quiescent development run is not used as
performance evidence.

The optimized Scala.js bundle contains runtime fields for the space, Ravel
data, and value-semantics witness. It contains no field for `S`, and ranked
`Sampled.apply` delegates directly to Ravel without an owner-proof wrapper.
`SamplingAlignment` exists only at a checked compatibility boundary.
`SomeSampled` retains its deliberate dynamic-boundary wrapper.

## Persistent record

The neutral record is:

```scala
final case class SampleSpaceRecord(
    grid: GridRecord,
    nonSpatialAxes: Vector[AxisRecord]
)
```

Live sample-space identity is reference identity. Persistent sampling identity
is the persistent grid key plus the ordered, coordinate-bearing axis records;
it does not need another randomly generated ID. Restoring a record requires a
live grid with the exact recorded persistent key and reconstructs a fresh live
sample-space owner. Independently restored owners interoperate only after
checked alignment.

Descriptive image metadata is not part of this record. Grid validation
tolerance is already excluded by `GridRecord`.

## Rejected alternatives

### Keep only frame ownership

Rejected because it cannot make exact pointwise operations total. It tracks a
weaker invariant than the operation requires.

### Add `S` while retaining public `F` and `D` parameters

`Sampled[S,F,D,A,Sem,R]` works, but the six-parameter principal type repeats
facts already carried by `S`. It increases generic signatures without adding
safety.

### Give `SampleSpace[S]` a separate allocated owner token

Rejected because the space value already has live identity. A second token
adds construction machinery and invites disagreement between two identity
owners. The singleton type of the space is sufficient and erases completely.

### Hide the owner only as an abstract member of `Sampled`

This can preserve owner capture, but same-space APIs require structural
refinements such as `{ type Space = left.Space }`. The explicit `S` parameter
produces clearer library signatures while ordinary user code still relies on
inference.

### Encode axes in a type-level tuple

Rejected. Runtime file loading, dynamic selection, and domain-specific axes
must remain an ordered runtime structure. The owner proves the complete
validated value without promoting every axis kind or extent into the type
system.

## Migration cost and public surface

The migration is source-breaking and intentionally occurs before API freeze.
It affects core aliases, `SomeSampled`, reference sampling, NIfTI signatures,
laws, and the locus adapter. Most application code remains source-shaped:

```scala
val axes = NonSpatialAxes.from(Vector(time)).toOption.get
val bold = Image.continuous(grid, axes, values).toOption.get
val volume = bold.atTime(17)
```

Generic library code changes from spelling frame and dimension parameters to
either:

- an inferred or named `S <: SampleSpace` when it needs same-owner algebra; or
- `Sampled[? <: SampleSpace, A, Sem, R]` when it only observes one image.

No owner object, registry lookup, allocation, or runtime compatibility branch
is added to ranked element access.
