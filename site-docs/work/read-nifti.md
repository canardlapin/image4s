# Read NIfTI data

The JVM NIfTI API accepts `java.nio.file.Path` and returns the decoded image,
parsed header, and affine-selection receipt together. Choose the read method by
what the next operation needs: scaled continuous values, native stored codes,
or categorical labels.

File examples use `mdoc:compile-only`. They must typecheck during the guide
build, but the build does not require example data on disk.

## Read scaled continuous values

Use `readScalar` for scaled `Double` values or `readScaledFloat` when `Float`
is sufficient and its precision policy is explicit.

```scala mdoc:compile-only
import image4s.*
import image4s.nifti.*
import java.nio.file.Path

val path = Path.of("subject01_bold.nii.gz")

val doubles: Either[
  NiftiError,
  DecodedNifti[SomeSampled[Double, Continuous]]
] = Nifti.readScalar(path)

val floats: Either[
  NiftiError,
  DecodedNifti[SomeSampled[Float, Continuous]]
] = Nifti.readScaledFloat(
  path,
  precision = NiftiFloatPrecision.RejectLossy
)
```

Both methods apply the effective NIfTI slope and intercept. `readScalar` uses
`Double`. `readScaledFloat` defaults to `RejectLossy`; choose
`AllowRounding` only when the application accepts values that cannot be
represented exactly as `Float`.

## Retain native stored codes

Use `readScalarStored` when an application needs the original supported dtype
and the encoding receipt rather than eagerly scaled continuous values.

```scala mdoc:compile-only
import image4s.nifti.*
import java.nio.file.Path

val path = Path.of("scanner_codes.nii")
val stored: Either[
  NiftiError,
  DecodedNifti[NiftiScalarStored]
] = Nifti.readScalarStored(path)
```

`NiftiScalarStored` records which native case was read: UInt8, Int16, Int32,
Float32, or Float64. Its codes are not silently presented as already-scaled
physical values. The retained header supplies the slope/intercept receipt for
the file interpretation.

## Read categorical labels

`readLabels` validates scaled values as finite integral `Long` label codes.
`readLabelsNative` is stricter: it retains UInt8, Int16, or Int32 storage and
requires no effective scaling.

```scala mdoc:compile-only
import image4s.*
import image4s.nifti.*
import java.nio.file.Path

val path = Path.of("atlas_labels.nii.gz")

val labels: Either[
  NiftiError,
  DecodedNifti[SomeSampled[Long, Categorical]]
] = Nifti.readLabels(path)

val nativeLabels: Either[
  NiftiError,
  DecodedNifti[NiftiLabelStored]
] = Nifti.readLabelsNative(path)
```

Choose `readLabels` when scaling may legitimately produce integral labels and
widening to `Long` is acceptable. Choose `readLabelsNative` when exact stored
integer codes and dtype matter. Native-label reading rejects floating storage
and effective scaling rather than changing the requested semantics.

## Keep the decode receipt

Every high-level read returns `DecodedNifti[I]`:

```scala mdoc:compile-only
import image4s.nifti.*
import java.nio.file.Path

val decoded = Nifti.readScalar(Path.of("subject01_bold.nii"))

val receipt = decoded.map { result =>
  (
    result.image,
    result.header,
    result.affineSelection.source,
    result.affineSelection.diagnostics
  )
}
```

`SomeSampled` hides rank-dependent details selected from the file header while
retaining the typed element and semantic role. The parsed `NiftiHeader` remains
available for provenance. `NiftiAffineSelection` records whether sform, qform,
fallback, or an explicit affine supplied the grid and retains disagreement
diagnostics.

## Choose affine and temporal-unit policies

```scala mdoc:compile-only
import image4s.geometry.LengthUnit
import image4s.nifti.*
import java.nio.file.Path

val options =
  NiftiReadOptions(
    fallbackSpatialUnit = LengthUnit.Millimeter,
    affinePolicy = NiftiAffinePolicy.RequireAgreement(1.0e-5),
    unknownTemporalUnit = NiftiUnknownTemporalUnitPolicy.Reject
  )

val checkedAffine =
  Nifti.readScalar(Path.of("subject01_bold.nii"), options)
```

The default affine policy prefers sform and retains diagnostics when qform and
sform disagree. `RequireAgreement` rejects disagreement beyond its tolerance.
For unknown temporal units, choose ordinal coordinates, rejection, or an
explicit seconds/milliseconds/microseconds assumption. The default is ordinal;
it does not guess a time unit.

## I/O and format limits

The current boundary supports NIfTI-1 single-file and pair-file storage for
UInt8, Int16, Int32, Float32, and Float64 payloads. JVM paths support plain and
gzip files. `Nifti.ioStrategy(path)` reports whether the selected path uses
bounded streaming or the whole-file compressed compatibility strategy.

`NiftiIoLimits` bounds working buffers, payload bytes, decoded bytes, and
extension bytes. Requests beyond those limits return `NiftiError`; they are not
allowed to allocate without a checked bound.

The Scala.js facade accepts Node.js string paths. It does not emulate browser
filesystem access. JVM mdoc examples prove JVM signatures only; the repository
cross-platform tests are the evidence for shared parser and Node parity.

## What changed?

| Read choice | Values | Spatial grid and axes | Semantic role | Receipt retained |
| --- | --- | --- | --- | --- |
| `readScalar` | Scaled to `Double` | Built from selected affine and header dimensions | Continuous | Header and affine selection |
| `readScaledFloat` | Scaled to `Float` under precision policy | Same decode policy | Continuous | Header and affine selection |
| `readScalarStored` | Native supported dtype | Same decode policy | Raw stored interpretation | Header, affine selection, encoding |
| `readLabels` | Validated integral `Long` codes | Same decode policy | Categorical | Header and affine selection |
| `readLabelsNative` | Native integral codes | Same decode policy | Categorical stored interpretation | Header, affine selection, encoding |

See [Supported scope and deliberate limits](../reference/supported-scope.md)
for the concise format boundary. To render a decoded image, continue to
[Display images with Intaglio](display-intaglio.md).
