package image4s.nifti

/**
 * Validated synchronous NIfTI-1 I/O for Node.js filesystem paths.
 *
 * This Scala.js API targets Node.js. It does not provide or emulate browser
 * filesystem access. Plain files use bounded working buffers. The synchronous
 * gzip compatibility path materializes the compressed stream and reports that
 * fact through `ioStrategy`; supported formats and conversion policies match
 * the JVM artifact.
 */
object Nifti:
  private val api = new NiftiApi[String](NodeNiftiFileSystem)

  export api.*
