# Supported scope and deliberate limits

This page separates the public model from current implementations and explicit
unsupported requests. image4s is pre-1.0 software on the `0.1.x` line; none of these statements is
a source- or binary-compatibility promise for a released 1.x line.

## Core representation

Current guarantees within the source contract:

- `Sampled` binds one Ravel array to one complete `SampleSpace`, metadata, and
  proven value semantics.
- Spatial axes precede ordered non-spatial axes in logical shape.
- D2 and D3 grids map a lattice isomorphically into a frame of the same
  dimensionality.
- Construction requires exact positive shape equality; broadcasting is not
  implicit.
- Continuous, categorical, and mask roles remain distinct from storage dtype.
- Runtime owner, persistent identity, exact congruence, approximate
  congruence, and value equality remain separate relations.

Deliberate limits:

- Native D2-in-D3 embedded planes are not represented by `Grid[D2]`; use a D3
  singleton extent or retain embedding geometry downstream.
- Axis units are identifiers, not an implicit unit-conversion system.
- image4s exposes no public flat-index or contiguous-storage contract.
- Approximate congruence does not authorize pointwise arithmetic.

## Exact views and resampling

Crop, spatial flip, spatial permutation, positive spatial stride, non-spatial
selection, and non-spatial permutation are exact views with derived sampling
descriptions. Canonicalization and explicit materialization copy logical values
without changing their sample space.

Arbitrary rotations, shears, nonlinear transforms, and interpolation onto a new
grid are not disguised as views. Production resampling belongs in reframe4s.

## Filters and morphology

Current implementations include direct and supported separable correlation,
convolution, Gaussian filtering, Sobel/Scharr gradients, thresholding, and
binary erosion/dilation/opening/closing over D2/D3 sampled images. Non-spatial
axes are processed as independent batches.

Current limits include:

- optimized separable execution for the supported `Same`-extent cases;
- explicit rejection of FFT filtering;
- explicit floating output for integer-backed filtering and gradients;
- D2 disk and D3 ball structuring elements;
- sequential use of prepared morphology plans that own reusable workspace.

## NIfTI

The current NIfTI-1 boundary supports scalar UInt8, Int16, Int32, Float32, and
Float64 payloads; supported single/pair-file layouts; implemented endianness,
extension, qform/sform, plain-file, and gzip cases; and explicit resource,
scaling, precision, label, affine, and unknown-temporal-unit policies.

Complex, RGB, binary, wider-integer, and NIfTI-2 payloads are rejected rather
than guessed. Scala.js filesystem access targets Node.js, not browsers.

## Intaglio display

The bridge supports D2 scalar rasters, axis-aligned regular fields, appearance
orientation, same-shape mask overlays, deterministic `Int` label palettes, and
orthogonal D3 source-grid slices.

It does not perform scientific filtering, resampling, oblique slicing,
multi-channel composition, or silent reinterpretation of rotated/sheared grids
as regular fields. Current mask overlay validation checks logical shape; the
caller must establish stronger sampling alignment when inputs come from
different sources.

## Evidence boundaries

- `docs/tlSite` proves JVM guide examples compile/evaluate and Laika renders.
- JVM/Scala.js test suites prove the configured cross-platform implementation
  surface, not external downstream compatibility.
- A local sibling-source override proves that checkout combination only. A
  clean pinned-source build is the release-relevant dependency gate.
- A configured benchmark, visual fixture, or CI matrix proves only what its
  executed assertions and environments cover.

See [Modules and platforms](modules-and-platforms.md) for where each capability
lives and [Errors and checked boundaries](errors-and-checked-boundaries.md) for
recovery from rejected requests.
