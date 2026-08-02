# Build and module boundaries

image4s is an sbt multi-project build. The shared image API is cross-compiled
for the JVM and Scala.js; filesystem adapters are platform-specific.

## Build the library

From the repository root:

```text
sbt -batch compileAll
sbt -batch image4sTestAll
```

The full test alias covers the cross-platform geometry, core, NIfTI,
reference, laws, locus, Intaglio, filter, morphology, and operation-law
projects. Focused commands are useful while writing a guide:

```text
sbt -batch "image4s-coreJVM / Test / testOnly image4s.ApproachableApiSuite"
sbt -batch "image4s-coreJS / Test / testOnly image4s.ApproachableApiSuite"
```

## Build the guide

The `docs` project lives in `site/` and reads the curated public source tree
from `site-docs/`. Its `tlSite` task runs mdoc first and then Laika:

```text
sbt -batch docs/tlSite
```

To work with a live preview while editing:

```text
sbt docs/tlSitePreview
```

The rendered files under `site/target/` are disposable build output. The
source markdown, directory navigation files, and build configuration are the
reviewable documentation surface.

## Source dependencies

Ordinary builds use the exact Ravel, Gale, locus4s, and Intaglio revisions
declared in `build.sbt`. When working on a sibling checkout, override it
explicitly:

```text
-Dimage4s.ravel.build=/absolute/path/to/ravel
-Dimage4s.gale.build=/absolute/path/to/gale
-Dimage4s.locus4s.build=/absolute/path/to/locus4s
```

The override is useful for development, but it is not the same evidence as a
fresh build against the pinned revisions.

## Where to look next

- `image4s-core` owns `Sampled`, `SampleSpace`, axes, metadata, semantics, and
  exact views.
- `image4s-filter` owns neighborhood filters and Gaussian operations.
- `image4s-nifti` owns the bounded NIfTI-1 format boundary and platform I/O.
- `image4s-reference` owns independent nearest and linear sampling oracles.

The repository README contains the current maturity statement and the full
development/evidence index.
