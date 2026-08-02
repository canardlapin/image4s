# NIfTI input and output

`image4s-nifti` is a deliberately bounded NIfTI-1 boundary. It supports the
implemented scalar storage types and makes scaling, affine choice, labels, and
I/O limits explicit.

## Read values or read storage

Use the high-level scalar methods when the pipeline wants continuous values:

```scala
Nifti.readScalar(path)
Nifti.readScaledFloat(path)
Nifti.readLabels(path)
```

Use `readRaw` or `readScalarStored` when the pipeline needs the native storage
codes and the header's value interpretation as a receipt. Raw codes are not
silently treated as already-scaled physical values.

The supported scalar payload subset is UInt8, Int16, Int32, Float32, and
Float64. Unsupported datatype families are rejected. Labels have a stricter
native path that accepts only integral storage without effective scaling.

## Affines and temporal units

`NiftiReadOptions` carries the affine policy. A reader can prefer sform or
qform, require their agreement within a tolerance, or supply an explicit
affine. Diagnostics remain attached to the decoded result when the header
contains a disagreement.

Unknown temporal units also have an explicit policy. Choose ordinal behavior,
an assumption, or rejection instead of allowing a guessed unit into the
sample space.

## Platform boundary

The shared parser and validation logic are reused on both platforms. The JVM
adapter accepts `java.nio.file.Path`; the Scala.js adapter targets Node.js
string paths. The Scala.js API does not emulate browser filesystem access.

Both adapters support the documented plain-file and gzip paths, but the
reported `NiftiIoStrategy` distinguishes bounded streaming from the whole-file
compressed compatibility path. That distinction matters for memory-sensitive
pipelines.

For the detailed format contract, limits, and parity evidence, see the
repository's [NIfTI implementation](https://github.com/canardlapin/image4s/tree/main/modules/image4s-nifti)
and [NIfTI tests](https://github.com/canardlapin/image4s/tree/main/modules/image4s-nifti/shared/src/test).
