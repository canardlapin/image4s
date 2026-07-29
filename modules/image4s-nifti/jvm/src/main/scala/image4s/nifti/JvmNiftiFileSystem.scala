package image4s.nifti

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream
import scala.util.control.NonFatal

private object JvmNiftiFileSystem extends NiftiFileSystem[Path]:
  def show(path: Path): String =
    path.toString

  def fileName(path: Path): String =
    Option(path.getFileName).fold("")(_.toString)

  def sibling(path: Path, fileName: String): Path =
    path.resolveSibling(fileName)

  def exists(path: Path): Boolean =
    Files.exists(path)

  def readBytes(
      path: Path,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]] =
    protect(path, operation) {
      val base =
        new BufferedInputStream(Files.newInputStream(path))
      val input: InputStream =
        if isCompressed(path) then new GZIPInputStream(base)
        else base
      try Right(input.readAllBytes())
      finally input.close()
    }

  def writeBytes(
      path: Path,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit] =
    protect(path, NiftiOperation.Write) {
      Option(path.getParent).foreach(Files.createDirectories(_))
      val base =
        new BufferedOutputStream(Files.newOutputStream(path))
      val output: OutputStream =
        if isCompressed(path) then new GZIPOutputStream(base)
        else base
      try
        output.write(bytes)
        Right(())
      finally output.close()
    }

  private def isCompressed(path: Path): Boolean =
    show(path).toLowerCase.endsWith(".gz")

  private def protect[A](
      path: Path,
      operation: NiftiOperation
  )(
      body: => Either[NiftiError, A]
  ): Either[NiftiError, A] =
    try body
    catch
      case NonFatal(error) =>
        Left(
          NiftiError.IoFailure(
            show(path),
            operation,
            Option(error.getMessage).getOrElse(error.getClass.getName)
          )
        )
