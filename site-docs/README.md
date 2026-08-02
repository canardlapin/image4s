# image4s guide

image4s is a typed Scala 3 library for multidimensional images. It keeps
sample values attached to their spatial grid, declared non-spatial axes,
metadata, and value semantics while they move through a pipeline.

This guide is for the current source checkout. image4s is early development
software (`0.1.0-SNAPSHOT`), so APIs and module boundaries can still change.
The examples are compiled by mdoc against the JVM modules during the site
build; a broken example fails the documentation build.

## Start here

- [Getting started](getting-started.md) builds one image, selects a time point,
  crops it, and maps its values.
- [The core model](core-model.md) explains how grids, non-spatial axes, and
  semantic value roles fit together.
- [Views and coordinates](guides/views.md) shows checked zero-copy spatial and
  non-spatial views.
- [Filtering](guides/filtering.md) adds a Gaussian operation without losing
  the sampled space.

## Reference

- [Build and module boundaries](reference/build.md)
- [NIfTI input and output](reference/nifti.md)

## Build this guide

From the repository root:

```text
sbt -batch docs/tlSite
```

The generated site is under `site/target/`. It is build output, not source,
and is intentionally not committed.
