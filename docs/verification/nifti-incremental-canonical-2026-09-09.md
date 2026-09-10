# Incremental output on the canonical image4s base

The verified incremental NIfTI writer and sample-space refinement were forward-ported onto image4s `18ffdce67fd05f5bcd28336650edcae891da6265` in `/private/tmp/image4s-output-canonical-1`. All tracked changes applied cleanly; six added source/test files complete the 12-path candidate. This is an uncommitted forward-port, separate from the earlier installed native candidate and its existing evidence.

Core tests pass on JVM / Scala.js: **96 / 92**. NIfTI tests pass on JVM / Node: **55 / 37**, including real-file Long offsets, conversion parity, sparse ordering and resource behavior. The fresh 160 MB public writer probe passed with a 64 MiB heap, writing and checking every one of 40 million values. Independent Python streaming checked all values and the analytic checksum 19,919,995,270; whole-file SHA-256 is `9585f6bdbbc9ff3f32567c33cd2df9c9f573b2216eb76ac3d3d2198e35700aac`.

The ordinary canonical ScalaFIM consumer at `10d14666edd06813cd909727f6b9a04b4ed41ba2`, plus its four-path bridge/refinement candidate, passes image **305 / 284**, dataset **76 / 62**, and fit **235 / 226** tests on JVM / Scala.js. Its independent byte tests preserve asymmetric FIR/voxel order, explicit units/affine codes and caller extensions. Full consumer evidence is in ScalaFIM `docs/verification/canonical-incremental-output-2026-09-09.md` and `.json.gz`.

The generic provider remains responsible only for physical staging output and encoding. ScalaFIM still owns estimate identities, axes/units, completeness and the durable result catalog. Recent selected-estimate scientific code on ScalaFIM's older branch has not been reconciled with the canonical image architecture, and no application pin has changed.

The provider bundle embeds exact base revisions, all 12 resulting source/test files, a replay-verified patch, source/config manifests, complete provider test logs, heap/Python receipts and the four-path consumer patch. Consumer testing used an exact local archive of its pinned reframe4s commit because a fresh remote clone could not resolve that commit. This is local JDK25/Node26 qualification, not hosted or dependency-publication proof. A temporary read-only console-Git setting accommodates sbt-git's JGit worktree limitation; no native build file or source pin was changed.

Incremental output is exclusive `.nii`, requires RAS, and uses first-axis-fastest spatial block indices. Header sampling follows explicit write options; richer axes, labels and nonzero FIR origins need caller metadata. Closing retains partial staging files and does not certify scientific completeness. Existing complete-image compressed/pair behavior remains covered by the provider suites.

Evidence: [nifti-incremental-canonical-2026-09-09.json.gz](nifti-incremental-canonical-2026-09-09.json.gz), SHA-256 `844e124ef0510d1966972df931024c4bcfc07c195a099c6b7cb3d5dd7a109541`.
