package image4s.nifti

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

private object NodeNiftiFileSystem extends NiftiFileSystem[String]:
  def show(path: String): String =
    path

  def fileName(path: String): String =
    NodePath.basename(path)

  def sibling(path: String, fileName: String): String =
    NodePath.join(NodePath.dirname(path), fileName)

  def exists(path: String): Boolean =
    NodeFs.existsSync(path)

  def readBytes(
      path: String,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]] =
    protect(path, operation) {
      val physical = NodeFs.readFileSync(path)
      val logical =
        if isCompressed(path) then NodeZlib.gunzipSync(physical)
        else physical
      Right(toScalaBytes(logical))
    }

  def writeBytes(
      path: String,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit] =
    protect(path, NiftiOperation.Write) {
      val parent = NodePath.dirname(path)
      NodeFs.mkdirSync(
        parent,
        js.Dynamic.literal(recursive = true)
      )
      val logical = toUint8Array(bytes)
      val physical =
        if isCompressed(path) then NodeZlib.gzipSync(logical)
        else logical
      NodeFs.writeFileSync(path, physical)
      Right(())
    }

  private def isCompressed(path: String): Boolean =
    path.toLowerCase.endsWith(".gz")

  private def toScalaBytes(bytes: Uint8Array): Array[Byte] =
    val output = new Array[Byte](bytes.length)
    var index = 0
    while index < output.length do
      output(index) = bytes(index).toByte
      index += 1
    output

  private def toUint8Array(bytes: Array[Byte]): Uint8Array =
    val output = new Uint8Array(bytes.length)
    var index = 0
    while index < bytes.length do
      output(index) = (bytes(index) & 0xff).toShort
      index += 1
    output

  private def protect[A](
      path: String,
      operation: NiftiOperation
  )(
      body: => Either[NiftiError, A]
  ): Either[NiftiError, A] =
    try body
    catch
      case error: Throwable =>
        Left(
          NiftiError.IoFailure(
            path,
            operation,
            Option(error.getMessage).getOrElse(error.getClass.getName)
          )
        )

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object NodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native

  def readFileSync(path: String): Uint8Array = js.native

  def mkdirSync(path: String, options: js.Object): Unit = js.native

  def writeFileSync(path: String, bytes: Uint8Array): Unit = js.native

@js.native
@JSImport("node:path",JSImport.Namespace)
private object NodePath extends js.Object:
  def basename(path: String): String = js.native

  def dirname(path: String): String = js.native

  def join(parts: String*): String = js.native

@js.native
@JSImport("node:zlib",JSImport.Namespace)
private object NodeZlib extends js.Object:
  def gunzipSync(bytes: Uint8Array): Uint8Array = js.native

  def gzipSync(bytes: Uint8Array): Uint8Array = js.native
