# image4s

image4s provides typed sampled-image foundations for Scala on the JVM and
Scala.js. It has one dense representation: `Sampled[F,D,A,Role,R]`, combining
an immutable Ravel `NDArray[A,R]` with a typed spatial grid, ordered
non-spatial axes, metadata, validity, and image-role semantics.

Repository boundaries are deliberately strict:

- Ravel owns dense storage, shapes, strides, layouts, views, builders, and
  numeric kernels.
- image4s owns spatial geometry, sampled-image semantics, basic NIfTI mechanics,
  reference sampling, and the optional finite-domain bridge.
- reframe4s owns transformations, production resampling, registration, fields,
  flows, and motion.
- locus4s owns identity-safe finite domains.
- ScalaFIM owns neuroimaging workflow and scientific policy.

Aliases such as scalar, component, label, and series images are roles or
zero-copy views over `Sampled`; they are not additional array containers.

## Artifacts

| Artifact | Ownership |
|---|---|
| `image4s-geometry` | dimensions, frames, points, vectors, grids, identities, and validated affine coordinates |
| `image4s-core` | `Sampled`, logical axes, roles, validity, metadata, checked construction, and ranked access |
| `image4s-nifti` | NIfTI-1 parsing and encoding plus JVM and Node.js filesystem/gzip adapters |
| `image4s-reference` | independent nearest and linear correctness oracles |
| `image4s-laws` | reusable image laws, representation contracts, and JVM performance court |
| `image4s-locus` | checked conversion from an image grid to a locus4s finite domain |

`image4s-core` has no filesystem or format API. The Scala.js NIfTI artifact
targets Node.js and does not claim browser filesystem support.

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
