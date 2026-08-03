# Errors and checked boundaries

image4s validates an image description when it is created or when two owners
meet. It does not repeat shape, affine, or alignment validation for every
sample access. Public checked operations return `Either`; direct ranked Ravel
indexing retains Ravel's indexing behavior.

## Construction and geometry

| User action | Error family | Typical cause | Next action |
| --- | --- | --- | --- |
| Parse an ID or create a frame | `GeometryError` | Empty ID/label or unsupported dimensional declaration | Correct the identifier or dimension |
| Create an affine | `GeometryError` | Wrong matrix size, non-finite coefficient, singular or ill-conditioned basis | Repair the transform or choose explicit diagnostic limits |
| Create a grid or index | `GeometryError` | Rank mismatch, non-positive extent, or out-of-bounds bounded index | Correct shape/index before constructing the image |
| Restore a frame/grid record | `GeometryError` | Key conflict, rank mismatch, or registry inconsistency | Reject the record or resolve the persistent identity conflict |

## Axes, images, and alignment

| User action | Error family | Typical cause | Next action |
| --- | --- | --- | --- |
| Create an axis | `ImageError` | Empty extent, zero step, non-finite coordinate, invalid label or unit ID | Correct the sampling declaration |
| Attach an array | `ImageError` | Array shape differs from `grid.shape ++ axes.shape` | Reshape explicitly or correct the sample space |
| Select time/channel/axis | `ImageError` | Missing or repeated axis kind, invalid axis position, or out-of-range index | Inspect declared axes and select an unambiguous coordinate |
| Create an exact view | `ImageError` | Crop bounds, invalid permutation, or non-positive stride | Correct the exact lattice request or use resampling |
| Align two sample spaces | `ImageError` | Frame mismatch, unequal shape/affine, or unequal ordered axis records | Reject, reconstruct correctly, or resample onto one chosen grid |
| Compare persistent spaces | `ImageError` | One or both spaces are ephemeral | Use live-owner or exact-congruence checks, or persist them deliberately |

Approximate congruence reports closeness only. It does not recover from an
exact-alignment error and cannot be passed to pointwise operations.

## Filtering and morphology

`OpError` reports invalid scales, incompatible kernels or extents, unsupported
execution methods, image construction failures, and unsupported geometry.

- Use positive finite Gaussian sigmas.
- Use non-negative morphology radii.
- Request floating output for integer-backed filtering or gradients.
- Use D2 disks and D3 balls in their supported dimensions.
- Treat an FFT request as unsupported until the API implements it.
- Use reframe4s when a requested transformation is not an exact view or
  supported separable operation.

## NIfTI

`NiftiError` distinguishes I/O failures, invalid headers, unsupported
datatypes/features, resource-limit failures, affine-policy failures, precision
loss, and invalid categorical values.

Choose the read method before handling the error:

- `readScalar` for scaled `Double` values;
- `readScaledFloat` with an explicit precision policy;
- `readScalarStored` for native codes;
- `readLabels` for checked integral `Long` labels; or
- `readLabelsNative` for unscaled native integral label codes.

Do not catch a broad exception and retry a different interpretation. A request
for labels or native codes is semantic intent; changing it should be an
explicit application decision.

## Intaglio display

`DisplayBridgeError` reports non-axis-aligned regular fields, degenerate field
axes, shape-incompatible overlays, invalid D3 slice indices, and downstream
Intaglio construction failures.

A rotated or sheared grid can still be rendered as a source-index raster. It
cannot be lowered to a regular axis-aligned field without explicit placement or
resampling.

For concrete examples of these boundaries, return to
[exact views](../work/exact-views.md),
[filtering](../work/filter-and-gradients.md), or
[NIfTI input](../work/read-nifti.md).
