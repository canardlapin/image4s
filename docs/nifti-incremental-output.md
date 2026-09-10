# Write an image as spatial blocks arrive

`image4s-nifti` provides `Nifti.openScalarWriter` and `Nifti.withScalarWriter`
for producers that cannot retain an entire image. They use the same NIfTI-1
header, extension, scaling and scalar-value encoder as `Nifti.writeScalar`.
No resident `Sampled` or `NDArray` is required.

The JVM entry point accepts `java.nio.file.Path`; the Scala.js entry point
accepts a Node.js path string. Incremental output supports uncompressed,
single-file `.nii` destinations. Gzip and pair-file incremental requests fail
with `UnsupportedIncrementalOutput`. The existing complete-image writers
continue to support their existing storage encodings.

## A map bundle with bounded memory

```scala
import image4s.*
import image4s.geometry.*
import image4s.nifti.*
import java.nio.file.Path

val frame = Frame.named[D3]("native", unit = LengthUnit.Millimeter, convention = CoordinateConvention.RAS).toOption.get
val grid = Grid.in(frame)(Vector(200, 200, 100), Affine.identity[D3]).toOption.get
val mapAxis = Axis.ordinal("map", AxisKind.Batch, 10).toOption.get
val axes = NonSpatialAxes.from(Vector(mapAxis)).toOption.get
val options = NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)

// Use a new staging path. The caller decides when the scientific result is complete.
val written = Nifti.withScalarWriter(Path.of("estimates-staging.nii"), grid, axes, options) { writer =>
  val firstBlock = Array.fill(8000)(1.5)
  writer.writeSpatialSpan(Vector(0), firstSpatialIndex = 0L, values = firstBlock)
}
```

This example writes only the first block of the first map. `Right` means those
writes and resource closure succeeded; it does not mean all maps were filled.
A production producer writes each of its blocks inside the scope and publishes
the file only after its own completeness checks succeed. The executable
`NiftiIncrementalHeapProbe` fills every sample of a 160 MB, ten-map file from
reused blocks under a 64 MiB heap, then checks every value using direct byte IO.

Each call selects one non-spatial coordinate in the declared `axes` order.
For a map axis and an echo axis, for example, `Vector(2, 1)` selects map 2 at
echo 1. Spatial indices use **NIfTI first-axis-fastest order**:

```text
spatial = x + nx * (y + ny * z)
```

These indices are not Ravel's default linear offsets. The first non-spatial
axis is also fastest among the non-spatial axes in the file. `Long` offsets
support files larger than a resident JVM or JavaScript array.

`writeSpatialSpan` accepts contiguous spatial samples without an index array.
`writeSpatialBlock` accepts `Array[Long]` spatial indices paired with
`Array[Double]` values. Blocks and sparse indices may arrive in any order;
adjacent indices are combined into bounded physical writes. Repeated indices
are overwritten in call order. Arrays are borrowed only for the synchronous
call and are not retained. Calls must be serialized and input arrays must not
be mutated during a call.

## Values, geometry and metadata

Incremental output requires an explicitly declared RAS frame. The supplied grid
and non-spatial axes remain attached to the writer. The
header uses the grid affine and spatial unit, and `NiftiWriteOptions` controls
datatype, slope/intercept, integer conversion and non-spatial header sampling.
These are the same options as for the complete-image writer. Rich axis labels,
explicit coordinates and scientific identities require extensions or an
accompanying manifest; the NIfTI header alone cannot express them. A declared
axis does not override `nonSpatialPixelDimensions` or `temporalUnit`.

Unwritten samples have **stored code zero**, which decodes to the declared
intercept. With default scaling this is numerical zero. If the application
requires another background value, it must write that value explicitly.

Use `withCoordinateSystem` to declare the sform reference. Its historical
default is `ScannerAnatomical`; `Mni152` writes code 4 and `TemplateOther`
writes code 5. An affine or frame name never selects a template implicitly.
`Unknown` writes code 0, which disables the sform. These declarations follow
the [NIfTI reference header](https://github.com/NIFTI-Imaging/nifti_clib/blob/master/niftilib/nifti1.h).
Only sform is written; qform remains unset, including for sheared affines.

## Resource and failure contract

- Creation is exclusive. Existing files, directories and symbolic links are
  not replaced. The caller supplies the staging location.
- Extents, representable byte sizes, configured resource limits and supported
  output mode are checked before file creation.
- Each block's indices and numeric conversions are validated before any of
  that block's bytes are written. A rejected block can be corrected and retried.
- A physical IO failure poisons and closes the writer. Later writes and close
  report the retained failure; a simultaneous close failure is retained too.
- `close` is idempotent. `withScalarWriter` closes on normal return, typed
  producer failure and thrown producer exceptions.
- Partial staging files remain after failure or early closure. Removal,
  coverage validation, atomic publication and crash recovery belong to the
  caller's lifecycle. Closing a format writer is not a durability certificate.

Memory is bounded by the configured working buffer, the supplied block and
bounded header/extensions, rather than the total image size. Node also uses
bounded typed-array conversion buffers. Neither platform falls back to a
whole-file allocation for this API.

## Verification

```text
sbt -batch "image4s-niftiJVM / test" "image4s-niftiJS / test"
```

The suites compare ordinary and incremental bytes, independently decode
headers and values, exercise sparse ordering and large offsets, inject
write/close failures, and run public JVM/Node IO with existing-target and
resource checks. Run the heap probe in a separate JVM with `-Xmx64m` and the
NIfTI test runtime classpath. Its one argument is a new `.nii` output path.

ScalaFIM owns first-level estimate identities, output availability, physical
response axes and scientific completeness. PLS Neuro owns application review,
execution state and PLS admission. Neither consumer needs another NIfTI codec.
