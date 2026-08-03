# Modules and platforms

image4s is a Scala 3 multi-project build. Add the modules needed by the
workflow; `image4s-core` deliberately does not depend on file formats, display,
or the operation catalogue.

## User-facing modules

| Module | Responsibility | JVM | Scala.js |
| --- | --- | :---: | :---: |
| `image4s-geometry` | D2/D3 frames, coordinates, affine grids, identity, and registries | Yes | Yes |
| `image4s-core` | `Sampled`, sample spaces, axes, metadata, semantic roles, conversion, and exact views | Yes | Yes |
| `image4s-filter` | Correlation, convolution, Gaussian filtering, and gradients | Yes | Yes |
| `image4s-morphology` | Thresholding and binary morphology | Yes | Yes |
| `image4s-nifti` | Bounded NIfTI-1 parsing, decoding, and writing | `Path` facade | Node.js string-path facade |
| `image4s-locus` | Checked conversion from grids to locus4s finite domains | Yes | Yes |
| `image4s-intaglio` | Display-only lowering to Intaglio fields and rasters | Yes | Yes |

`image4s-reference` provides independent nearest and linear sampling oracles.
It is useful for conformance work rather than ordinary application pipelines.

## Development and law modules

| Module | Responsibility |
| --- | --- |
| `image4s-ops-core` | Shared operation vocabulary: kernels, borders, extents, scales, and execution policies |
| `image4s-laws` | Reusable laws for the core representation and geometry |
| `image4s-ops-laws` | Operation conformance, visual QA, parity benchmarks, and allocation courts |

The operation modules are temporarily hosted in this repository so their
dependency boundary remains executable. The permanent rule is unchanged:
`image4s-core` must never depend on `image4s-ops-core`, filtering, or
morphology.

## Platform boundaries

Shared image, geometry, filter, morphology, NIfTI codec, and display code is
cross-compiled for the JVM and Scala.js. A JVM documentation build compiles the
examples on the JVM classpath; it is not proof of Scala.js behavior. The
repository's JVM/Scala.js test gate supplies that evidence.

Filesystem access is platform-specific:

- JVM NIfTI methods accept `java.nio.file.Path`.
- Scala.js NIfTI methods accept Node.js string paths.
- Browser filesystem access is not emulated.

For implemented format and algorithm bounds, see
[Supported scope and deliberate limits](supported-scope.md). Contributor build
commands are under [Build and check the guide](../contribute/build-guide.md).
