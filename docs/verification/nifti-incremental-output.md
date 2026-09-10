# Incremental NIfTI output: native candidate evidence

Verified 8 September 2026 for image4s issue `bd-01M21J17MKGV12VQKYR99XCR8F`.
This is an uncommitted native candidate; ScalaFIM adoption and the first-level
saved-output lifecycle remain pending.

## Result

- All **54 JVM** and **36 Scala.js/Node** NIfTI tests passed, with no failures.
- A separate `-Xmx64m` JVM wrote **160,000,000 payload bytes** and checked all
  **40,000,000 values** using bounded direct byte IO: **2.012164875 seconds**
  for writing plus verification in this run.
- A separate Python `struct` streaming check independently verified every
  value and the analytical checksum **19,919,995,270**.
- Public JVM and Node writers created sparse files of **8,589,410,664 bytes**,
  verified unwritten zero and the last sample beyond 32-bit byte addressing,
  and closed and removed those test files.
- Existing complete-image scalar, label, gzip, pair-file, extension, read,
  semantic and allocation tests passed in the same final module suites.

The 160 MB file SHA-256 is `9585f6bdbbc9ff3f32567c33cd2df9c9f573b2216eb76ac3d3d2198e35700aac`. Its total
length, including header, is 160,000,352 bytes. This fixture demonstrates bounded
memory; it is not a GLM benchmark or a promise about storage-device durability.

## Exact inputs and receipts

Native base: `3fb9fd6dc385a1cd7021f7d8a7157efb7e1c78a3`. The isolated candidate copied the
pre-existing native `build.sbt` changes without altering them. Its build hash is
`a79a1f0b28893fd39e513a14fbdcd40dbf1aadb804935349729a19465cda39e9`. Source and build input-set hash:
`6b9d09a5ce768830cc168a2e3ca37bc370bb28c3930def00061a7220619d8c7f` (114 embedded inputs).

The [compressed evidence bundle](nifti-incremental-output.json.gz) includes the
changed files, all candidate Scala/build inputs, exact provider revisions,
command/output logs, heap command and independent byte-check receipt. Bundle
SHA-256: `37b574da0bc057cdb9cdd46fd2ffaf0f1fad178a66fae37b8a29c2dda140510f`; 240,540 bytes.

Final test command (after scoped formatting):

```text
sbt -batch -Dsbt.global.base=/private/tmp/image4s-incremental-sbt-global-1 \
  "image4s-niftiJVM / test" "image4s-niftiJS / test"
```

The [public guide](../nifti-incremental-output.md) describes ordering, options,
resource scope and staging-file responsibilities. The shared suite compares
ordinary and incremental bytes across all five datatypes, sparse/repeated
indices, multi-axis order, conversion refusal, scaling overflow, failure and
close handling, limits and coordinate declarations. Platform suites use actual
JVM/Node file IO, exclusive destination creation, scoped closure and large
offsets. The heap probe uses the ordinary public `Nifti` entry point.

## Limits and remaining work

Local runtime: macOS arm64, Homebrew Java 25.0.1, Node 26.7.0, Scala 3.7.4,
sbt 1.11.7. Hosted Ubuntu JDK 17/21 gates, the full image4s repository test matrix,
publication and consumer admission were not run or claimed. The isolated copy
has no Git metadata; sbt's nonfatal revision-detection diagnostics are retained
in the logs, while the hashes above identify the actual source.

The final native code reuses one scalar encoder and header encoder. Its new
incremental API requires an explicit RAS frame; the complete-image writer's
historical frame behavior remains unchanged. The shared Float32 encoder now
rejects finite values whose scaling itself overflows, with a regression through
both writer APIs. Earlier compiler/test failures and their corrections remain
in the bundle.

ScalaFIM adoption is `bd-01M21H6P1908Q5PGSZYWQZHWRN`. Its currently pinned image4s
revision `497bfd164ad514ff3d1944699550c78caa57e85d` is materially older than the native
base. Adoption must qualify geometry/axis API changes and explicit output units;
this evidence is not permission to mechanically update pins. ScalaFIM owns
scientific map identity, completeness and manifests. PLS Neuro W5 owns saved
estimate-set review/reuse and PLS admission. Both remain required work.
