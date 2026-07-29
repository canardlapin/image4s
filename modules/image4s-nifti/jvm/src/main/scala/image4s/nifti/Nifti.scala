package image4s.nifti

import java.nio.file.Path

/** Basic synchronous NIfTI-1 I/O for JVM filesystem paths. */
object Nifti:
  private val api = new NiftiApi[Path](JvmNiftiFileSystem)

  export api.*
