# Shared Spatial4s coordinate authority — 2026-10-06

Baseline Image4s: `5bba168f97b26e50c4c154cb3854266c0b1765af`.
Spatial4s: `2eca2b379c1985881e4ca152b2a6c63a763277b0`, integrated by
[Spatial4s PR 1](https://github.com/canardlapin/spatial4s/pull/1).
Gale: `85a8d12023b49598e5f39705fe8127cebbd9c014`.

Image4s coordinate names now alias Spatial4s dimensions, frames, points, vectors,
keys, records, units and conventions. Image4s retains grids, index-to-world
geometry, sampled images and IO. Static factory errors remain GeometryError;
shared instance methods return SpatialError with an explicit fromCoordinate
bridge. Restoration in one registry returns the canonical frame object.

The owning regressions check exact type equality, a Spatial4s point on an
Image4s grid, finite computed affine outputs, unsupported custom NIfTI units
before any file is created, unchanged standard persistent fingerprints and
non-colliding custom-unit fingerprints. The affine overflow regression failed
on the baseline operator, then passed on both platforms after the correction.
Existing signed-INT8, temporal-origin and overlap behavior is retained.

Engineering qualification on macOS 12.7.6 x86_64, Temurin JDK 21.0.12 and
Node 24.21.0: all 86 test suites, 679 tests, passed across JVM and Scala.js.
`fmt fmtCheck` passed. `docs/tlSite` compiled 21 mdoc files with zero errors
and rendered 15 documents. Commands were executed sequentially, using an
explicit `-Dimage4s.spatial4s.build` override to a clean checkout at the exact
integrated Spatial4s revision above. Ordinary builds use that same remote pin.

The successful courts cover geometry/core/NIfTI/reference/laws first, followed
by locus/intaglio/ops/filter/morphology/ops-laws, with updated geometry repeated
for custom-unit encoding. Upstream CI runs `image4sCompileAll image4sTestAll`,
JDK 17/21, optimized Scala.js, docs and compatibility policy against remote pins.

This is engineering conformance, not qualification of neuroimaging workflows
or complete-workload performance. Reframe4s and ephys4s are the affected
consumer checks; ephys4s tracks this work as
`ephys4s-provider-image-spatial-authority` and
`ephys4s-provider-image-affine-finite`. Separate ScalaFIM/application migration
and scientific checks are not claimed by this provider change.
