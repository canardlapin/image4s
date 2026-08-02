# image4s

image4s provides typed sampled-image foundations for Scala on the JVM and
Scala.js. It has one dense representation: `Sampled[S,A,Sem,R]`, combining
an immutable Ravel `NDArray[A,R]` with a typed spatial grid, ordered
non-spatial axes, metadata, validity, and proven value semantics. `S` is the
complete live sample-space owner, so exact pointwise compatibility can be
proved once and reused.

Repository boundaries are deliberately strict:

- Ravel owns dense storage, shapes, strides, layouts, views, builders, and
  numeric kernels.
- image4s owns spatial geometry, sampled-image semantics, basic NIfTI mechanics,
  reference sampling, and the optional finite-domain bridge.
- reframe4s owns transformations, production resampling, registration, fields,
  flows, and motion.
- locus4s owns identity-safe finite domains.
- ScalaFIM owns neuroimaging workflow and scientific policy.

`ContinuousImage`, `CategoricalImage`, and `MaskImage` are aliases of
`Sampled`. Continuous and categorical aliases are generic in the element type;
`MaskImage` is Boolean support and is not a 0/1 label image.
`TimeSeriesView` and `ComponentAxisView` are checked zero-copy semantic views;
they prove a unique declared axis without creating another data container.

## Artifacts

| Artifact | Ownership |
|---|---|
| `image4s-geometry` | dimensions, frames, points, vectors, grids, identities, and validated affine coordinates |
| `image4s-core` | `Sampled`, logical axes, value semantics, semantic views, validity, metadata, checked construction, and ranked access |
| `image4s-ops-core` | shared ops vocabulary (borders, extents, supports, plans); temporarily hosted here for extraction |
| `image4s-filter` | convolution and neighborhood filters over `Sampled` |
| `image4s-morphology` | binary and grayscale morphology over `Sampled` |
| `image4s-ops-laws` | reusable ops laws and engine conformance suites |
| `image4s-nifti` | NIfTI-1 parsing and encoding plus JVM and Node.js filesystem/gzip adapters |
| `image4s-reference` | independent nearest and linear correctness oracles |
| `image4s-laws` | reusable image laws, representation contracts, and JVM performance court |
| `image4s-locus` | checked conversion from an image grid to a locus4s finite domain |
| `image4s-intaglio` | display-only lowering of D2 continuous images into Intaglio fields and rasters |

`image4s-core` has no filesystem, format, or filter catalogue API. Ops modules
depend on core; core never depends on ops. The Scala.js NIfTI artifact targets
Node.js and does not claim browser filesystem support.

## Numeric storage conversion

Use `Sampled.convertTo[B]` when an image needs a different numeric storage
dtype without changing its grid, declared non-spatial axes, metadata, or value
semantic role. It returns `Either[ImageError, Sampled[...]]`; an
`Overflow.Reject` failure is reported as `ImageError.NumericConversion`.

```scala
val labels: CategoricalImage[?, Int, ?] = ???
val bytes = labels.convertTo[Byte](
  ConversionPolicy(overflow = Overflow.Clamp)
)
```

The default policy uses nearest-even rounding and rejects values outside the
target range. Use `ConversionPolicy` to request toward-zero, floor, or ceiling
rounding and reject, clamp, or explicit low-level wrap behavior. Ravel performs
the successful conversion in primitive storage; image4s does not construct a
boxed value collection.

## Linear filtering

`image4s-filter` preserves `Float` or `Double` storage when the input already
uses a floating dtype:

```scala
import image4s.filter.*
import image4s.ops.*

val sigma = SpatialSigma.samples[D2](1.5)
val blurred = sigma.flatMap(image.gaussianBlur(_))
```

Integer-backed continuous images require an explicit floating output:

```scala
val blurredFloat = sigma.flatMap(image.gaussianBlurTo[Float](_))
```

The preserving method is unavailable for integer storage, so filtering cannot
silently truncate into `Byte`, `Short`, `Int`, or `Long`. Both methods process
non-spatial axes independently and preserve the exact grid for `Same` extent.
Sample-space sigma always produces a separable Gaussian. Frame-space sigma is
also separable for axis-aligned grids and for isotropic kernels on orthogonal
grids; anisotropic rotated or sheared cases return `OpError.InvalidScale`.

Call `Gaussian.prepare` or `Gaussian.prepareTo` when applying the same filter
to several images in the same live `SampleSpace`. The returned sequential plan
reuses its primitive destination workspace and Ravel address schedule. Each
`run` still returns a fresh immutable image, so later runs cannot change
earlier results. Create one plan per concurrent caller.

## Encoded values

`EncodedSampled` retains one Ravel buffer plus a structural `ValueEncoding`.
It does not introduce another image storage type. `valueAt` decodes a domain
value on demand; `materializeTo` creates a regular `Sampled` when downstream
code needs decoded storage.

The v1 encodings are `Identity`, `UniformAffine`, `PerAxisAffine`, and a
deterministic string `Codebook`. Encodings contain only data, expose stable
fingerprints, and never accept user callbacks. Spatial crops remain zero-copy
and retain the same encoding; `PerAxisAffine` coefficients remain aligned to
their declared non-spatial axis.

## NIfTI semantic reads

NIfTI decoding makes storage conversion explicit:

```scala
val stored  = Nifti.readScalarStored(path)
val floats  = Nifti.readScalarAs(
  path,
  NiftiValueConversion.ScaledFloat(NiftiFloatPrecision.RejectLossy)
)
val doubles = Nifti.readScalar(path)
val labels  = Nifti.readLabels(path)
```

`readScalarStored` returns a `NiftiScalarStored` case with the native codes and
their `ValueEncoding`. UInt8, Int16, Int32, Float32, and Float64 use Ravel
`UInt8`, `Short`, `Int`, `Float`, and `Double` storage with identity
interpretation. It does not apply the NIfTI
slope/intercept. The returned `DecodedNifti` retains the header that declares
that scaling.

`readScalarAs` materializes scaled values at a requested floating precision.
`readScalar` is the `Double` convenience spelling. `readScaledDouble`,
`readScaledFloat`, `readAs`, and `readRaw` remain available as compatibility
names. The Float reader rejects precision loss by default and requires
`NiftiFloatPrecision.AllowRounding` to narrow deliberately.

Writes require `NiftiWriteOptions.forDatatype(...)`; its slope and intercept
state the storage encoding instead of inferring one from image values.

`readLabels` applies declared scaling only when every result remains an exact
finite `Long` category. Fractional, non-finite, or out-of-range labels are
typed failures. `writeLabels` likewise accepts `Long` categories and integral
NIfTI storage only.

`readLabelsNative` is stricter. It returns `NiftiLabelStored` with UInt8,
Int16, or Int32 codes and their encoding receipt, without widening to `Long`.
It rejects Float32/Float64 payloads and non-identity header scaling. A zero
header slope is accepted because NIfTI defines it as "do not scale."

The fourth dimension becomes a regular `Time` axis when the header declares
seconds, milliseconds, or microseconds. Unknown units follow
`NiftiUnknownTemporalUnitPolicy`; later axes remain ordinal unless the caller
adds domain semantics.

Affine choice belongs to `NiftiReadOptions`: prefer sform, prefer qform,
require agreement under an explicit tolerance, or supply an explicit affine.
Every `DecodedNifti` retains the original `NiftiHeader` plus an
`NiftiAffineSelection` receipt and qform/sform disagreement diagnostics.

The codec supports the NIfTI-1 UInt8, Int16, Int32, Float32, and Float64
datatype subset; little- and big-endian input; single and pair files; gzip;
extension blocks; qform and sform. Complex, RGB, binary, wider integer, and
NIfTI-2 payloads are rejected rather than guessed.

JVM reads and writes, including gzip, use a bounded reusable working buffer and
decode directly into the one final Ravel-owned array. Node.js uses the same
bounded path for uncompressed files; its synchronous gzip adapter currently
reports `WholeFileCompressedCompatibility` through `Nifti.ioStrategy(path)`.
Callers can inspect this strategy before I/O. `NiftiIoLimits` bounds the working
buffer, encoded payload, decoded output, and extension region, with typed
failures before allocation or output when a limit is exceeded. Memory mapping
and out-of-core storage remain Ravel concerns, not reasons to introduce a
second image representation.

## Intaglio display bridge

`image4s-intaglio` lowers a D2 continuous `Sampled` image to Intaglio without
turning display choices into image-processing operations.

```scala
val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 1.0))
val field = DisplayBridge.toIntaglioField(image)
val raster = DisplayBridge.renderRaster(image, plan)
```

`toIntaglioField` accepts only axis-aligned, separable affines. Reflected axes
are displayed in increasing Intaglio-axis order; rotated and sheared grids
return `DisplayBridgeError.NonAxisAlignedField` rather than being silently
reinterpreted. Resample those grids explicitly with reframe4s, or render a
raster and place it in an Intaglio scene using the corresponding transform.

`renderRaster` is always available for the D2 scalar surface. It applies the
window, palette, and pixel-space orientation while directly packing Intaglio's
primitive RGBA raster. It does not create a transformed `Sampled` image.

## Logical indexing

Logical image axes are spatial axes followed by non-spatial axes:

```text
(i, j)
(i, j, k)
(i, j, k, t)
```

For a three-dimensional grid, `k` is the third grid coordinate and therefore
the slice index. Anatomical/world direction comes from the grid affine, not
from the array position. Ravel alone owns physical storage order.

## Non-spatial sampling

Every non-spatial axis declares the coordinates at which its values were
sampled. Coordinates may be ordinal, regular numeric samples, explicit numeric
samples, or categorical labels:

```scala
val time =
  Axis.regular(
    name = "time",
    kind = AxisKind.Time,
    extent = 300,
    origin = 0.0,
    step = 0.8,
    unit = AxisUnit.Seconds
  )

val echo =
  Axis.explicit(
    name = "echo",
    kind = AxisKind.Echo,
    values = Vector(12.0, 28.0, 44.0),
    unit = AxisUnit.Milliseconds
  )
```

`Axis.create(name, extent, kind)` remains an ordinal-axis constructor. Use
`Axis.regular`, `Axis.explicit`, or `Axis.categorical` when the coordinates
carry sampling meaning. `AxisRecord` is a validated-round-trip structural
record; it does not create persistent identity. Downstream domains can add
stable custom kind and unit identifiers without adding cases to image4s.

## Value semantics

Value structure is separate from axis structure. `Sampled.continuous` is
generic in its element type and requires a `LinearInterpolable[A]` capability.
`Sampled.categorical` accepts exact categorical values such as integral label
codes. `ReferenceSampler.nearest` preserves the stored element type. Linear
sampling requires `LinearSampling[A,Sem]` and an explicit
`ReferenceSampler.linearToFloat` or `ReferenceSampler.linearToDouble` call;
it accumulates in Double and never performs implicit Byte or Short arithmetic.

Downstream libraries may define their own semantic tag and opt into
`ValueSemantics[A,Sem]` or `LinearSampling[A,Sem]` without extending a closed
image-role hierarchy. A time series or component-bearing image is established
by `TimeSeriesView.from(image)` or
`ComponentAxisView.from(image, kind)`, both of which reject missing or
ambiguous axes and retain the original `Sampled` value by reference.

## An ordinary image workflow

The proof machinery stays behind inferred owners and checked constructors:

```scala
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray

val frame =
  Frame.named[D3](
    "native",
    unit = LengthUnit.Millimeter,
    convention = CoordinateConvention.RAS
  ).toOption.get
val grid =
  Grid.in(frame)(
    Vector(6, 7, 5),
    Affine.identity[D3]
  ).toOption.get
val time =
  Axis.regular(
    "time",
    AxisKind.Time,
    extent = 4,
    origin = 0.0,
    step = 0.8,
    unit = AxisUnit.Seconds
  ).toOption.get
val axes = NonSpatialAxes.from(Vector(time)).toOption.get
val values =
  NDArray.tabulate[Double](6, 7, 5, 4)((i, j, k, t) =>
    1000.0 * i + 100.0 * j + 10.0 * k + t
  )

val bold = Image.continuous(grid, axes, values).toOption.get
val volume = bold.atTime(2).toOption.get
val crop =
  volume
    .crop(Vector(1, 2, 1), Vector(3, 4, 2))
    .toOption
    .get
val centered = crop.mapValues(_ - 1000.0)
```

Two fields constructed from one explicit `SampleSpace` have the same inferred
owner and combine directly with `zipWith`. Independently reconstructed spaces
first produce `SamplingAlignment`; `rebind` shares the immutable Ravel array,
and `zipWithAligned` consumes the same evidence without rechecking:

```scala
val sum = left.zipWith(sameOwner)(_ + _)
val alignment = left.sampleSpace.alignExact(right.sampleSpace).toOption.get
val checkedSum = left.zipWithAligned(right, alignment)(_ + _)
```

The corresponding construction, selection, crop, map, alignment, rebind, and
zip workflow is compiled and run on JVM and Scala.js by
`ApproachableApiSuite`.

## Exact views and field algebra

`LatticeMap[D]` is an exact target-to-source integer-affine map restricted to
signed strides and axis permutations. Every constructible map is therefore a
zero-copy Ravel view. Its target grid affine is derived by composition, so
physical coordinates and logical values cannot drift apart:

```scala
val crop       = image.spatialView(Vector(8, 8, 4), Vector(80, 80, 48))
val flipped    = image.flipSpatial(axis = 0)
val reoriented = image.permuteSpatial(Vector(2, 1, 0))
val sparseGrid = image.strideSpatial(Vector(2, 2, 1))
```

`LatticeMap.identity`, `crop`, `flip`, `permute`, `stride`, and
`stridedPermutation` are checked constructors; `followedBy` composes exact
pullbacks. Arbitrary index maps are not disguised as views. A caller needing a
broader transform must materialize explicitly or resample through reframe4s.

`mapValues` preserves the element type and proven value semantics;
`mapValuesAs` requires explicit evidence when either changes. `zipWith` is
total for equal-typed images with the same static sample-space owner;
`zipWithAligned` consumes one reusable exact alignment proof. The `As`
variants expose changed output types and semantics. `replaceDataChecked` and
`replaceAfterNonSpatialReduction` let downstream Ravel kernels return storage
while image4s revalidates the resulting sampling shape. Non-spatial selection
and permutation always move declared coordinates with their data axes.

## Geometry identity

Ordinary constructors are pure and ephemeral:

```scala
val frame =
  Frame.named[D3](
    "native",
    unit = LengthUnit.Millimeter,
    convention = CoordinateConvention.RAS
  )
val grid = frame.flatMap(value =>
  Grid.in(value)(Vector(96, 96, 60), affine)
)
```

Ephemeral frames and grids have live owner identity but no persistent record.
Persistence is explicit: parse or supply a project ID, call
`Frame.persistentNamed` or `Grid.createPersistent`, and pass the resulting
record through an immutable registry. `FrameKey` contains rank, unit, and
coordinate convention; `GridKey` contains the frame key, shape, and canonical
affine values. Display labels and affine validation tolerances are not key
material. Restoring through separate registries deliberately creates distinct
live owners that can be aligned by their common persistent key.

Spatial geometry is deliberately equal-dimensional: `Affine[D]` /
`AffineIso[D]` and `Grid[F,D]` mean D2-in-D2 or D3-in-D3. A future native
D2-in-D3 plane will use a separately named embedding type rather than changing
those coordinates. Direction/spacing construction checks orthonormal direction
cosines, and every affine exposes inverse-residual and condition diagnostics.
Use `LatticeIndex` for an unbounded integer coordinate and a grid-created
`GridIndex` when finite bounds and live-grid ownership must be proved.

## Build

```text
sbt compileAll
sbt testAll
```

The build uses Scala 3.7.4 and cross-publishes every artifact for the JVM and
Scala.js. `image4s-nifti` executes its Scala.js filesystem tests under Node.js.

During coordinated development, immutable source dependencies may be replaced
explicitly:

```text
-Dimage4s.ravel.build=/absolute/path/to/ravel
-Dimage4s.gale.build=/absolute/path/to/gale
-Dimage4s.locus4s.build=/absolute/path/to/locus4s
```

Ordinary builds use the exact revisions declared in `build.sbt`.
