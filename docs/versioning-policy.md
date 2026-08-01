# Versioning and compatibility policy

image4s uses early semantic versioning and checks compatibility with
`sbt-version-policy`, which includes MiMa binary checks.

The current `0.1.0-SNAPSHOT` build is the first public baseline. It therefore
declares `Compatibility.None`: there is no older image4s artifact against
which a meaningful compatibility claim can be made. CI still runs
`versionPolicyCheck` so the policy and task do not silently disappear.

Immediately after publishing `0.1.0`, every later change must declare its
intended compatibility:

- Patch releases use `Compatibility.BinaryAndSourceCompatible`.
- Compatible feature releases use `Compatibility.BinaryCompatible`.
- Deliberate breaking releases use `Compatibility.None` and advance the
  version as required by early-semver.

CI compatibility success is a check against published predecessors, not proof
that an unreleased API should be frozen. The architecture epic intentionally
keeps the central public types unstable until its P0 decisions and semantic
laws are complete.
