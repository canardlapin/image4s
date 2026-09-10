# Canonical provider alignment — 2026-09-10

Merged `accea0496c9bcd7690093216426e5403add0ef33` into canonical
`18ffdce67fd05f5bcd28336650edcae891da6265`, retaining main's axis,
native-storage, selected-sampling and Ravel changes. Gale is pinned to
`83cac90a678d1b8a31c590e0c1b8fc8bf3427161`.

All 12 implementation/test files in the temporary estimate-output provider
match the merge byte for byte. The temporary checkout is no longer required
once consumers adopt this merge.

Local standalone-clone validation with Java 25.0.1 and the checked-in sbt:

| Suite group | JVM | Scala.js |
|---|---:|---:|
| core | 96 | 92 |
| NIfTI | 55 | 37 |
| locus | 38 | 36 |

All 354 tests passed without compiler warnings, using pinned dependencies and
no local provider override. This is local source evidence, not a release or
remote CI certificate.
