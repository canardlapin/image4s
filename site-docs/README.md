# image4s

image4s represents multidimensional images whose coordinates and meaning stay
attached to their values.

```text
image = values + spatial grid + sampled axes + value meaning
```

This combination changes what an image operation is allowed to assume:

- A crop remains in the same physical coordinate frame. Its grid records where
  the first cropped sample lies.
- A time point comes from a declared time axis. Code does not have to guess
  that the fourth array dimension means time.
- A label image remains categorical even when its stored codes are numbers.
  Continuous-only operations cannot accept it by accident.
- Equal array shapes do not prove that two images sample the same locations.
  Pointwise work requires a shared sample space or an exact alignment proof.
- Ordinary construction can be declared once with `SamplingSpec`; the request
  expands to the same canonical frame, grid, axes, and sample space.

image4s is early-development Scala 3 software (`0.1.0-SNAPSHOT`). The public
model and module boundaries are deliberate, but source and binary compatibility
are not yet promised. The executable examples in this guide compile against
the JVM modules during `docs/tlSite`.

## Where image4s fits

image4s owns sampled-image meaning: geometry, declared axes, semantic roles,
metadata, and checked image operations. Neighboring libraries have narrower
jobs:

| Library | Responsibility |
| --- | --- |
| Ravel | Dense arrays, layouts, storage views, and numerical kernels |
| Gale | Coordinate frames and affine geometry used by image4s |
| reframe4s | Resampling values onto a different grid |
| Intaglio | Turning display descriptions into rendered output |

## Choose a route

- **Build a first image:** follow [Getting started](getting-started.md).
- **Reduce common imports:** opt into `image4s.prelude.*`; import `NDArray` and
  feature modules separately so ownership remains visible.
- **Understand the representation:** read
  [The sampled-image model](understand/sampled-image-model.md), then
  [Geometry and sampled axes](understand/geometry-and-axes.md).
- **Complete a task:** start with
  [exact views](work/exact-views.md),
  [filtering](work/filter-and-gradients.md),
  [morphology](work/masks-and-morphology.md),
  [NIfTI input](work/read-nifti.md), or
  [Intaglio display](work/display-intaglio.md).
- **Integrate reconstructed data:** read
  [Identity, reconstruction, and alignment](deeper/identity-and-alignment.md).

The compact [reference](reference/modules-and-platforms.md) records module,
platform, error, and support boundaries. Contributor build commands live under
[Build and check the guide](contribute/build-guide.md).
