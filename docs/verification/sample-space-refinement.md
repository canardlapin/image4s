# Checked sample-space dimension refinement

Verified 9 September 2026, image4s `bd-01M23PZVZKGA7CY5A2T8F17WSN`.

`SomeSampleSpace.requireD2` and `requireD3` recover a dimension-specific sampling
space without copying or rebuilding it. The result retains the exact live space,
grid, frame, units, convention and non-spatial axes. Its frame type remains
existential. Mismatched dimensions return `SpatialDimensionMismatch`; refinement
does not manufacture persistent identity or align unrelated frames.

The cast stays inside image4s, where sealed dimensions and immutable sample spaces
justify the checked refinement. Consumer code requires no cast. Tests cover both
ranks, wrong-rank refusals, exact reference preservation, physical declarations,
non-spatial sampling and separation of equal-looking ephemeral owners.

## Verification

- Core: 89 JVM and 85 Scala.js tests pass.
- NIfTI: 54 JVM and 36 Scala.js/Node tests pass, including real large-file offsets.
- Both focused refinement tests pass again on each platform after scoped formatting.
- Existing incremental writer sources are unchanged. The earlier 160 MB/64 MiB
  heap and Python byte probes were not rerun; their historical evidence is retained.

The [evidence bundle](sample-space-refinement.json.gz) embeds all 115 candidate
compilation inputs, dependency revisions and full logs. SHA-256: `c81a7b34c4e9a6b2a5960d802c4d84676e84e51d65ab3b409715a1abd50e81cf`.
Base incremental bundle SHA-256: `37b574da0bc057cdb9cdd46fd2ffaf0f1fad178a66fae37b8a29c2dda140510f`.
The tested candidate is `/private/tmp/image4s-output-refinement-1`.

## Consumer boundary

ScalaFIM's migrated image module compiles but fails its identity-sensitive tests.
Its unchanged baseline passes 247 JVM and 234 JS tests. Native migration issue
`bd-01M23Q9Q92SBD5PHZZJYVY9EC3` owns that separate compatibility work; the
[ScalaFIM adoption record](../../../scalafim/docs/verification/image4s-output-adoption-2026-09-09.md)
describes it. This provider fix does not admit a dependency pin, establish the
scientific output catalog or complete the saved-estimate application workflow.
No commit, publication, hosted or full-repository acceptance is claimed.
