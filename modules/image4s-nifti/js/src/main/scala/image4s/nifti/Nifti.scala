package image4s.nifti

/**
 * Basic synchronous NIfTI-1 I/O for Node.js filesystem paths.
 *
 * This Scala.js API targets Node.js. It does not provide or emulate browser
 * filesystem access.
 */
object Nifti:
  private val api = new NiftiApi[String](NodeNiftiFileSystem)

  export api.*
