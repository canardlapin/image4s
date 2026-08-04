# Versioning and compatibility policy

image4s uses early semantic versioning and publishes all public modules at one
shared version. The first published baseline is `0.1.0`; untagged development
builds use the version derived from Git by `sbt-dynver` until that release is
cut.

`0.1.0` establishes a reproducible dependency coordinate, not a stable API
promise. The README and supported-scope guide must continue to describe the
project as pre-1.0 until the central image and geometry contracts are mature
enough for a `1.0.0` support commitment.

## Release identity

- A release is an immutable `vX.Y.Z` or `vX.Y.Z-QUALIFIER` tag on `main`.
- The tag is the source of truth for the published version; untagged builds
  are development snapshots and are not a downstream default.
- Every published image4s module uses the same version. The aggregate root,
  documentation site, and `image4s-ops-laws` test/benchmark module are not
  published.
- A version is published at most once. Corrections require a new patch version.
- Release candidates use tags such as `v0.1.0-RC1` and must be tested through
  the same release workflow as the final release.

## Compatibility contract

The compatibility level is declared in `build.sbt` with
`versionPolicyIntention` and checked by `sbt-version-policy`.

### Pre-1.0

- `0.1.z` is reserved for changes that are source-compatible,
  binary-compatible, and semantically compatible, including bug fixes,
  documentation, and performance improvements.
- A public API, default behavior, ownership rule, coordinate meaning, or
  other semantic contract break advances the base version to `0.(y+1).0` and
  uses `Compatibility.None`.
- Significant additive features may also advance the base version when that
  gives downstream projects a clearer migration point.

### 1.0 and later

- Patch releases require `Compatibility.BinaryAndSourceCompatible`.
- Minor releases require `Compatibility.BinaryCompatible`; source changes may
  require downstream edits but existing compiled consumers must remain valid.
- Major releases use `Compatibility.None` and may break binary or semantic
  compatibility.

MiMa and the version-policy plugin are evidence for API compatibility, not a
substitute for semantic review. Changes to image identity, sampling, geometry,
storage ownership, defaults, or numerical behavior require explicit release
notes and focused behavioral tests even when MiMa is clean.

## Consumer pinning

After `0.1.0` is published, downstream repositories should use exact Maven
coordinates, for example:

```scala
val image4sVersion = "0.1.0"

libraryDependencies +=
  "io.github.canardlapin" %%% "image4s-core" % image4sVersion
```

Do not use dynamic versions, default snapshots, or source `ProjectRef`s in an
ordinary downstream build. An explicit local-source property may remain for
coordinated development and must be named, reviewed, and opt-in.

## Release gates

The tag workflow must:

1. require a semantic `v` tag;
2. run formatting, JVM and Scala.js tests, optimized Scala.js/Node tests, and
   the executable documentation build;
3. run `versionPolicyCheck` and `versionCheck` before publication;
4. generate every public POM and reject snapshot or dynver upstream
   dependencies;
5. publish signed artifacts through `ci-release` to Maven Central.

After publication, the release owner must verify that a clean consumer can
resolve the intended JVM and Scala.js artifacts at the exact release version.
That post-publication check is part of closing the release, even though it is
separate from the upload job because Central propagation is asynchronous.

The source `ProjectRef`s used by the current development build are therefore
not release evidence. Before tagging `0.1.0`, Ravel, Gale, locus4s, and
Intaglio dependencies used by public image4s modules must have stable published
coordinates, and the release POM rehearsal must show those coordinates.

The first release intentionally uses `Compatibility.None` because no previous
image4s artifact exists. Immediately after publishing `0.1.0`, the next
development state must move to `0.1.1-SNAPSHOT` and
`Compatibility.BinaryAndSourceCompatible`. A breaking change must move that
development state to the next `0.y` base and declare `Compatibility.None` in
the same reviewed change.
