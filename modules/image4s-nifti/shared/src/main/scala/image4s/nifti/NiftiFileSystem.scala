package image4s.nifti

/**
 * Platform boundary for basic synchronous NIfTI filesystem operations.
 *
 * Format parsing, validation, byte order, storage variants, and numeric
 * conversion remain in shared code. JVM and Node.js implementations own only
 * path handling, physical file access, and gzip.
 */
private[nifti] trait NiftiFileSystem[P]:
  def show(path: P): String

  def fileName(path: P): String

  def sibling(path: P, fileName: String): P

  def exists(path: P): Boolean

  def readBytes(
      path: P,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]]

  def writeBytes(
      path: P,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit]
