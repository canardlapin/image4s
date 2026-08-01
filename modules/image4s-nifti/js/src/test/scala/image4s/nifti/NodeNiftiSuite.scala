package image4s.nifti

import image4s.Axis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.Sampled
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

final class NodeNiftiSuite extends FunSuite:
  private var temporaryDirectory = ""

  override def beforeAll(): Unit =
    temporaryDirectory =
      NodeTestFs.mkdtempSync(
        NodeTestPath.join(NodeTestOs.tmpdir(), "image4s-nifti-node-")
      )

  override def afterAll(): Unit =
    if temporaryDirectory.nonEmpty then
      NodeTestFs.rmSync(
        temporaryDirectory,
        js.Dynamic.literal(recursive = true, force = true)
      )

  test("Node round trips every datatype across all storage encodings"):
    val cases =
      Vector(
        NiftiDatatype.UInt8 -> Vector(0.0, 17.0, 255.0),
        NiftiDatatype.Int16 -> Vector(-32768.0, 0.0, 32767.0),
        NiftiDatatype.Int32 -> Vector(-100000.0, 0.0, 100000.0),
        NiftiDatatype.Float32 -> Vector(-2.5, 0.0, 1.25),
        NiftiDatatype.Float64 -> Vector(-2.25, 0.125, 9.5)
      )
    val suffixes =
      Vector(".nii", ".nii.gz", ".hdr", ".hdr.gz")

    cases.foreach { case (datatype, values) =>
      suffixes.foreach { suffix =>
        val image = scalarImage(s"$datatype-$suffix", values)
        val path = temporaryPath(s"$datatype$suffix")
        val written =
          niftiRight(
            Nifti.writeScalar(
              path,
              image,
              NiftiWriteOptions.forDatatype(datatype)
            )
          )

        val expectedStorage =
          if suffix.startsWith(".nii") then NiftiStorage.SingleFile
          else NiftiStorage.PairFile
        assertEquals(
          niftiRight(Nifti.readHeader(path)).storage,
          expectedStorage
        )
        assertEquals(
          niftiRight(Nifti.readHeader(path)).datatype,
          datatype
        )
        written match
          case NiftiFiles.SingleFile(singlePath) =>
            assertEquals(expectedStorage, NiftiStorage.SingleFile)
            assertEquals(singlePath, path)
          case NiftiFiles.PairFile(headerPath, imagePath) =>
            assertEquals(expectedStorage, NiftiStorage.PairFile)
            val entry =
              if suffix.endsWith(".gz") then
                path.replace(".hdr.gz", ".img.gz")
              else path.replace(".hdr", ".img")
            assert(NodeTestFs.existsSync(headerPath))
            assert(NodeTestFs.existsSync(imagePath))
            assertDecodedValues(entry, values)

        assertDecodedValues(path, values)
      }
    }

  test("Node preserves extensions in gzip single and pair files"):
    val extensions =
      Vector(
        extensionRight(NiftiExtension.create(6, Vector[Byte](1, 2, 3))),
        extensionRight(
          NiftiExtension.create(42, Vector[Byte](9, 8, 7, 6, 5))
        )
      )
    val image = scalarImage("extensions", Vector(1.0, 2.0))

    Vector(".nii.gz", ".hdr.gz").foreach { suffix =>
      val path = temporaryPath(s"extensions$suffix")
      niftiRight(
        Nifti.writeScalar(
          path,
          image,
          extensions = extensions
        )
      )
      assertEquals(
        niftiRight(Nifti.readHeader(path)).extensions,
        extensions
      )
      val bytes = NodeTestFs.readFileSync(path)
      assertEquals(bytes(0).toInt, 0x1f)
      assertEquals(bytes(1).toInt, 0x8b)
    }

  test("Node round trips exact labels across single, pair, and gzip storage"):
    val suffixes = Vector(".nii", ".nii.gz", ".hdr", ".hdr.gz")
    val expected = Vector(-32768L, 0L, 32767L)

    suffixes.foreach { suffix =>
      val path = temporaryPath(s"labels$suffix")
      niftiRight(
        Nifti.writeLabels(
          path,
          labelImage(s"labels-$suffix", expected),
          NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
        )
      )
      assertEquals(
        sampledValues(niftiRight(Nifti.readLabels(path)).image),
        expected
      )
    }

  test("Node reports missing pair companions with physical paths"):
    val path = temporaryPath("missing.hdr")
    val image = scalarImage("missing", Vector(1.0))
    val written = niftiRight(Nifti.writeScalar(path, image))
    val imagePath =
      written match
        case NiftiFiles.PairFile(_, payload) => payload
        case _ => fail("expected a pair-file result")
    NodeTestFs.rmSync(imagePath)

    assertEquals(
      Nifti.readHeader(path),
      Left(NiftiError.MissingCompanion(path, imagePath))
    )

  test("Node rejects lossy integer output before creating a file"):
    val path = temporaryPath("lossy.nii.gz")
    val image = scalarImage("lossy", Vector(1.5))
    val result =
      Nifti.writeScalar(
        path,
        image,
        NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
      )

    result match
      case Left(_: NiftiError.ValueNotRepresentable) => ()
      case other => fail(s"expected ValueNotRepresentable, got $other")
    assert(!NodeTestFs.existsSync(path))

  test("Node preserves non-spatial sampling and temporal units"):
    val frame = rasFrame("timing")
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3])
      )
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.fromSeq(Shape(1, 1, 1, 2), Vector(1.0, 2.0))
        )
      )
    val options =
      NiftiWriteOptions.default
        .withNonSpatialSampling(
          Vector(0.8),
          NiftiTemporalUnit.Second
        )
        .fold(error => fail(error.message), identity)
    val path = temporaryPath("timing.nii.gz")

    niftiRight(Nifti.writeScalar(path, image, options))
    val header = niftiRight(Nifti.readHeader(path))
    val decoded = niftiRight(Nifti.readScaledDouble(path))

    assertEqualsDouble(header.pixelDimensions(3), 0.8f.toDouble, 0.0)
    assertEquals(header.temporalUnit, NiftiTemporalUnit.Second)
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        val axis =
          d3.value.nonSpatialAxes(0).getOrElse(fail("missing time axis"))
        assertEquals(axis.kind, AxisKind.Time)
        assertEquals(
          imageRight(axis.coordinateAt(1)),
          image4s.AxisCoordinate.Numeric(
            0.8f.toDouble,
            image4s.AxisUnit.Seconds
          )
        )
    )

  test("Node reports bounded uncompressed and buffered gzip strategies"):
    assertEquals(
      Nifti.ioStrategy(temporaryPath("strategy.nii")),
      NiftiIoStrategy.BoundedStreaming
    )
    assertEquals(
      Nifti.ioStrategy(temporaryPath("strategy.nii.gz")),
      NiftiIoStrategy.WholeFileCompressedCompatibility
    )

  private def assertDecodedValues(
      path: String,
      expected: Vector[Double]
  ): Unit =
    val decoded = niftiRight(Nifti.readScaledDouble(path))
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        expected.indices.foreach { index =>
          assertEqualsDouble(
            imageRight(d3.value.valueAt(Vector(index, 0, 0))),
            expected(index),
            1e-6
          )
        }
    )
    val raw = niftiRight(Nifti.readRaw(path))
    assertEquals(rawAsDoubles(raw.image), expected)

  private def sampledValues[A, Sem](
      sampled: image4s.SomeSampled[A, Sem]
  ): Vector[A] =
    sampled.fold(
      _ => fail("NIfTI must produce D3"),
      d3 => d3.value.data.elementsIterator.toVector
    )

  private def rawAsDoubles(raw: NiftiRawImage): Vector[Double] =
    raw match
      case NiftiRawImage.UInt8(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Int16(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Int32(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Float32(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Float64(image) =>
        sampledValues(image)

  private def scalarImage(
      label: String,
      values: Vector[Double]
  ) =
    val frame = rasFrame(label)
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(values.length, 1, 1),
          Affine.identity[D3]
        )
      )
    imageRight(
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.fromSeq(
          Shape(values.length, 1, 1),
          values
        )
      )
    )

  private def labelImage(
      label: String,
      values: Vector[Long]
  ) =
    val frame = rasFrame(label)
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(values.length, 1, 1),
          Affine.identity[D3]
        )
      )
    imageRight(
      Sampled.categorical(
        grid,
        NonSpatialAxes.empty,
        NDArray.fromSeq(
          Shape(values.length, 1, 1),
          values
        )
      )
    )

  private def temporaryPath(name: String): String =
    NodeTestPath.join(temporaryDirectory, name)

  private def rasFrame(label: String): Frame[D3] =
    geometryRight(
      Frame.named[D3](
        label,
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](
      value: Either[image4s.ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def extensionRight(
      value: Either[NiftiExtensionError, NiftiExtension]
  ): NiftiExtension =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def niftiRight[A](
      value: Either[NiftiError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object NodeTestFs extends js.Object:
  def existsSync(path: String): Boolean = js.native

  def mkdtempSync(prefix: String): String = js.native

  def readFileSync(path: String): Uint8Array = js.native

  def rmSync(path: String): Unit = js.native

  def rmSync(path: String, options: js.Object): Unit = js.native

@js.native
@JSImport("node:os",JSImport.Namespace)
private object NodeTestOs extends js.Object:
  def tmpdir(): String = js.native

@js.native
@JSImport("node:path",JSImport.Namespace)
private object NodeTestPath extends js.Object:
  def join(parts: String*): String = js.native
