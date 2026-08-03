package image4s.nifti

import java.nio.file.Path

/** Validated synchronous NIfTI-1 I/O for JVM filesystem paths.
  *
  * Header, extension, and payload I/O use bounded working buffers for both plain and gzip paths.
  * The supported datatype and feature subset, conversion policies, resource limits, and memory
  * boundary are documented in the project README.
  */
object Nifti:
  private val api = new NiftiApi[Path](JvmNiftiFileSystem)

  export api.*
