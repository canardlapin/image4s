package image4s.nifti

import image4s.Categorical
import image4s.AxisCoordinate
import image4s.AxisUnit
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.Continuous
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

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream

final class NiftiSuite extends FunSuite:
  test("scalar reads translate first-axis-fastest file order"):
    val path = temporaryPath("asymmetric.nii")
    writeFixture(
      path,
      dimensions = Vector(2, 3, 1),
      values = Vector(0.0, 100.0, 10.0, 110.0, 20.0, 120.0)
    )

    val decoded = niftiRight(Nifti.readScaledDouble(path))
    val checked =
      decoded.image.fold(
        _ => fail("NIfTI must produce D3"),
        d3 =>
          assertEquals(d3.value.logicalShape, Vector(2, 3, 1))
          assertEquals(
            imageValue(d3.value.valueAt(Vector(0, 0, 0))),
            0.0
          )
          assertEquals(
            imageValue(d3.value.valueAt(Vector(1, 0, 0))),
            100.0
          )
          assertEquals(
            imageValue(d3.value.valueAt(Vector(0, 2, 0))),
            20.0
          )
          assertEquals(
            imageValue(d3.value.valueAt(Vector(1, 2, 0))),
            120.0
          )
          true
      )

    assert(checked)

  test("scalar and label entry points preserve an explicit role"):
    val path = temporaryPath("roles.nii")
    writeFixture(
      path,
      dimensions = Vector(2, 1, 1),
      values = Vector(1.0, 2.0)
    )

    val scalar: DecodedNifti[image4s.SomeSampled[Double, Continuous]] =
      niftiRight(Nifti.readScaledDouble(path))
    val labels: DecodedNifti[image4s.SomeSampled[Long, Categorical]] =
      niftiRight(Nifti.readLabels(path))

    assertEquals(scalar.image.storageRank, 3)
    assertEquals(labels.image.storageRank, 3)

  test("supported datatypes apply slope and intercept"):
    val cases =
      Vector(
        NiftiDatatype.UInt8 -> 200.0,
        NiftiDatatype.Int16 -> -1234.0,
        NiftiDatatype.Int32 -> 123456.0,
        NiftiDatatype.Float32 -> 1.25,
        NiftiDatatype.Float64 -> -2.5
      )

    cases.zipWithIndex.foreach { case ((datatype, raw), index) =>
      val path = temporaryPath(s"datatype-$index.nii")
      writeFixture(
        path,
        dimensions = Vector(1, 1, 1),
        datatype = datatype,
        slope = 2.0,
        intercept = 3.0,
        values = Vector(raw)
      )
      val decoded = niftiRight(Nifti.readScaledDouble(path))
      decoded.image.fold(
        _ => fail("NIfTI must produce D3"),
        d3 =>
          assertEqualsDouble(
            imageValue(d3.value.valueAt(Vector(0, 0, 0))),
            raw * 2.0 + 3.0,
            1e-6
          )
      )
    }

  test("big-endian payloads are decoded with their header order"):
    val path = temporaryPath("big-endian.nii")
    writeFixture(
      path,
      dimensions = Vector(2, 1, 1),
      datatype = NiftiDatatype.Int16,
      order = NiftiByteOrder.BigEndian,
      values = Vector(-32000.0, 1234.0)
    )

    val decoded = niftiRight(Nifti.readScaledDouble(path))
    assertEquals(decoded.header.byteOrder, NiftiByteOrder.BigEndian)
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(
          imageValue(d3.value.valueAt(Vector(0, 0, 0))),
          -32000.0
        )
        assertEquals(
          imageValue(d3.value.valueAt(Vector(1, 0, 0))),
          1234.0
        )
    )

  test("qform honors qfac and sform has explicit precedence"):
    val qformPath = temporaryPath("qform.nii")
    val halfSqrt = math.sqrt(0.5)
    writeFixture(
      qformPath,
      dimensions = Vector(1, 1, 1),
      spacing = Vector(2.0, 3.0, 4.0),
      qform =
        Some(
          QForm(
            0.0,
            0.0,
            halfSqrt,
            Vector(10.0, 20.0, 30.0),
            qfac = -1.0
          )
        ),
      values = Vector(7.0)
    )
    val qformDecoded =
      niftiRight(Nifti.readScaledDouble(qformPath))
    assertRows(
      qformDecoded.affineSelection.affine.rowMajor,
      Vector(
        0.0, -3.0, 0.0, 10.0,
        2.0, 0.0, 0.0, 20.0,
        0.0, 0.0, -4.0, 30.0,
        0.0, 0.0, 0.0, 1.0
      )
    )

    val sform =
      Vector(
        -2.0, 0.25, 0.0, 40.0,
        0.0, 3.0, 0.0, 50.0,
        0.0, 0.0, 4.0, 60.0,
        0.0, 0.0, 0.0, 1.0
      )
    val bothPath = temporaryPath("both-forms.nii")
    writeFixture(
      bothPath,
      dimensions = Vector(1, 1, 1),
      qform =
        Some(
          QForm(
            0.0,
            0.0,
            0.0,
            Vector(1.0, 2.0, 3.0),
            qfac = 1.0
          )
        ),
      sform = Some(sform),
      values = Vector(1.0)
    )
    val bothDecoded =
      niftiRight(Nifti.readScaledDouble(bothPath))
    assertRows(bothDecoded.affineSelection.affine.rowMajor, sform)

  test("missing qform and sform use the pixel-dimension fallback"):
    val path = temporaryPath("fallback.nii")
    writeFixture(
      path,
      dimensions = Vector(1, 1, 1),
      spacing = Vector(2.0, 3.0, 4.0),
      values = Vector(1.0)
    )

    val decoded = niftiRight(Nifti.readScaledDouble(path))
    assertRows(
      decoded.affineSelection.affine.rowMajor,
      Vector(
        2.0, 0.0, 0.0, 0.0,
        0.0, 3.0, 0.0, 0.0,
        0.0, 0.0, 4.0, 0.0,
        0.0, 0.0, 0.0, 1.0
      )
    )

  test("gzip input is supported"):
    val path = temporaryPath("compressed.nii.gz")
    writeFixture(
      path,
      dimensions = Vector(1, 1, 1),
      values = Vector(42.0),
      compressed = true
    )

    val decoded = niftiRight(Nifti.readScaledDouble(path))
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(
          imageValue(d3.value.valueAt(Vector(0, 0, 0))),
          42.0
        )
    )

  test("unknown spatial units use the caller-visible fallback policy"):
    val path = temporaryPath("unknown-unit.nii")
    writeFixture(
      path,
      dimensions = Vector(1, 1, 1),
      spatialUnitCode = 0,
      values = Vector(1.0)
    )

    val default = niftiRight(Nifti.readScaledDouble(path))
    val meters =
      niftiRight(
        Nifti.readScaledDouble(
          path,
          NiftiReadOptions(LengthUnit.Meter)
        )
      )

    default.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(
          d3.value.frame.unit,
          LengthUnit.Millimeter
        )
    )
    meters.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(d3.value.frame.unit, LengthUnit.Meter)
    )

  test("readIn preserves the caller-supplied frame owner"):
    val path = temporaryPath("supplied-frame.nii")
    writeFixture(
      path,
      dimensions = Vector(1, 1, 1),
      values = Vector(5.0)
    )
    val frame = rasFrame("shared-RAS")

    val decoded = niftiRight(Nifti.readScaledDoubleIn(path, frame))

    assert(decoded.image.frame eq frame)

  test("readIn rejects incompatible supplied frame metadata"):
    val path = temporaryPath("incompatible-frame.nii")
    writeFixture(
      path,
      dimensions = Vector(1, 1, 1),
      values = Vector(5.0)
    )
    val unspecified =
      geometryRight(Frame.named[D3]("unspecified"))
    val meters =
      geometryRight(
        Frame.named[D3](
          "meters",
          LengthUnit.Meter,
          CoordinateConvention.RAS
        )
      )

    assertEquals(
      Nifti.readScaledDoubleIn(path, unspecified),
      Left(
        NiftiError.FrameConventionMismatch(
          CoordinateConvention.Unspecified
        )
      )
    )
    assertEquals(
      Nifti.readScaledDoubleIn(path, meters),
      Left(
        NiftiError.FrameUnitMismatch(
          NiftiSpatialUnit.Millimeter,
          LengthUnit.Meter
        )
      )
    )

  test("uncompressed writes preserve affine, indices, and NIfTI file order"):
    val path = temporaryPath("roundtrip.nii")
    val frame = rasFrame("writer")
    val affine =
      geometryRight(
        Affine.fromRowMajor[D3](
          Vector(
            -2.0, 0.25, 0.0, 40.0,
            0.0, 3.0, 0.0, 50.0,
            0.0, 0.0, 4.0, 60.0,
            0.0, 0.0, 0.0, 1.0
          )
        )
      )
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3, 1), affine))
    val time = imageRight(image4s.Axis.create("time", 2, image4s.AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val data =
      NDArray.fromSeq(
        Shape(2, 3, 1, 2),
        for
          x <- 0 until 2
          y <- 0 until 3
          z <- 0 until 1
          t <- 0 until 2
        yield 1000.0 * x + 100.0 * y + 10.0 * z + t
      )
    val image =
      imageRight(Sampled.continuous(grid, axes, data))
    val writeOptions =
      writeOptionsRight(
        NiftiWriteOptions.default.withNonSpatialSampling(
          Vector(1.75),
          NiftiTemporalUnit.Second
        )
      )

    niftiRight(Nifti.writeScalar(path, image, writeOptions))

    val raw = Files.readAllBytes(path)
    val rawBuffer = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN)
    assertEquals(rawBuffer.getFloat(92), 1.75f)
    assertEquals(rawBuffer.get(123), 10.toByte)
    val expectedFileOrder =
      for
        t <- 0 until 2
        z <- 0 until 1
        y <- 0 until 3
        x <- 0 until 2
      yield 1000.0 * x + 100.0 * y + 10.0 * z + t
    expectedFileOrder.zipWithIndex.foreach { case (expected, index) =>
      assertEqualsDouble(
        rawBuffer.getDouble(352 + index * 8),
        expected,
        0.0
      )
    }

    val decoded = niftiRight(Nifti.readScaledDoubleIn(path, frame))
    assertRows(
      decoded.affineSelection.affine.rowMajor,
      affine.rowMajor,
      1e-5
    )
    assertEquals(decoded.header.pixelDimensions(3), 1.75)
    assertEquals(decoded.header.temporalUnit, NiftiTemporalUnit.Second)
    assertEquals(
      decoded.image.nonSpatialAxes(0).map(_.kind),
      Some(image4s.AxisKind.Time)
    )
    assertEquals(
      decoded.image.nonSpatialAxes(0).map(_.coordinateAt(1)),
      Some(
        Right(
          AxisCoordinate.Numeric(1.75, AxisUnit.Seconds)
        )
      )
    )
    for
      x <- 0 until 2
      y <- 0 until 3
      t <- 0 until 2
    do
      assertEquals(
        imageValue(decoded.image.valueAt(Vector(x, y, 0), Vector(t))),
        1000.0 * x + 100.0 * y + t
      )

    val invalidOptions =
      writeOptionsRight(
        NiftiWriteOptions.default.withNonSpatialSampling(
          Vector(1.0, 2.0),
          NiftiTemporalUnit.Second
        )
      )
    assertEquals(
      Nifti.writeScalar(
        temporaryPath("too-many-pixdims.nii"),
        image,
        invalidOptions
      ),
      Left(NiftiError.NonSpatialPixelDimensionCount(2, 1))
    )

  test("label writes use the same Sampled owner and remain labels on read"):
    val path = temporaryPath("labels-roundtrip.nii")
    val frame = rasFrame("label-writer")
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 1, 1), Affine.identity[D3]))
    val labels =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 1, 1), Vector(3L, 9L))
        )
      )

    niftiRight(
      Nifti.writeLabels(
        path,
        labels,
        NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
      )
    )
    val decoded: DecodedNifti[
      image4s.SomeSampled[Long, Categorical]
    ] =
      niftiRight(Nifti.readLabels(path))

    assertEquals(decoded.header.datatype, NiftiDatatype.Int16)
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(
          imageValue(d3.value.valueAt(Vector(0, 0, 0))),
          3L
        )
        assertEquals(
          imageValue(d3.value.valueAt(Vector(1, 0, 0))),
          9L
        )
    )

  test("gzip single-file output is explicit and round trips"):
    val path = temporaryPath("output.nii.gz")
    val frame = rasFrame("gzip-writer")
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 1, 1), Affine.identity[D3]))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 1, 1), Vector(4.0, 8.0))
        )
      )

    assertEquals(
      niftiRight(Nifti.writeScalar(path, image)),
      NiftiFiles.SingleFile(path)
    )
    val compressed = Files.readAllBytes(path)
    assertEquals(compressed.take(2).toVector, Vector(0x1f.toByte, 0x8b.toByte))

    val decoded = niftiRight(Nifti.readScaledDouble(path))
    assertEquals(decoded.header.storage, NiftiStorage.SingleFile)
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(
          imageValue(d3.value.valueAt(Vector(1, 0, 0))),
          8.0
        )
    )

  test("pair files round trip through either member path"):
    val headerPath = temporaryPath("pair.hdr")
    val imagePath = headerPath.resolveSibling("pair.img")
    val frame = rasFrame("pair-writer")
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 1, 1), Affine.identity[D3]))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 1, 1), Vector(2.0, 6.0))
        )
      )

    assertEquals(
      niftiRight(Nifti.writeScalar(headerPath, image)),
      NiftiFiles.PairFile(headerPath, imagePath)
    )
    val headerBytes = Files.readAllBytes(headerPath)
    assertEquals(
      new String(headerBytes, 344, 3, java.nio.charset.StandardCharsets.US_ASCII),
      "ni1"
    )
    assertEquals(headerBytes.length, 352)
    assertEquals(Files.size(imagePath), 16L)
    Files.write(headerPath, headerBytes.take(348))

    val throughHeader = niftiRight(Nifti.readScaledDouble(headerPath))
    val throughImage = niftiRight(Nifti.readScaledDouble(imagePath))
    assertEquals(throughHeader.header.storage, NiftiStorage.PairFile)
    Vector(throughHeader, throughImage).foreach { decoded =>
      decoded.image.fold(
        _ => fail("NIfTI must produce D3"),
        d3 =>
          assertEquals(
            imageValue(d3.value.valueAt(Vector(1, 0, 0))),
            6.0
          )
      )
    }

  test("gzip-compressed pair files round trip through either member path"):
    val imagePath = temporaryPath("compressed-pair.img.gz")
    val headerPath = imagePath.resolveSibling("compressed-pair.hdr.gz")
    val frame = rasFrame("compressed-pair-writer")
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3]))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(1, 1, 1), Vector(17.0))
        )
      )

    assertEquals(
      niftiRight(Nifti.writeScalar(imagePath, image)),
      NiftiFiles.PairFile(headerPath, imagePath)
    )
    Vector(headerPath, imagePath).foreach { physicalPath =>
      assertEquals(
        Files.readAllBytes(physicalPath).take(2).toVector,
        Vector(0x1f.toByte, 0x8b.toByte)
      )
    }
    Vector(headerPath, imagePath).foreach { entry =>
      val decoded = niftiRight(Nifti.readScaledDouble(entry))
      decoded.image.fold(
        _ => fail("NIfTI must produce D3"),
        d3 =>
          assertEquals(
            imageValue(d3.value.valueAt(Vector(0, 0, 0))),
            17.0
          )
      )
    }

  test("all numeric encodings are emitted across physical storage variants"):
    val frame = rasFrame("numeric-matrix")
    val image = scalarImage(frame, Vector(0.0, 1.0, 2.0))
    val variants =
      Vector(
        "numeric.nii",
        "numeric.nii.gz",
        "numeric.hdr",
        "numeric.img.gz"
      )

    for
      datatype <- NiftiDatatype.values.toVector
      (name, variantIndex) <- variants.zipWithIndex
    do
      val path =
        temporaryPath(s"${datatype.toString.toLowerCase}-$variantIndex-$name")
      val options = NiftiWriteOptions.forDatatype(datatype)
      val files =
        niftiRight(Nifti.writeScalar(path, image, options))
      val (headerPath, payloadPath, payloadOffset) =
        files match
          case NiftiFiles.SingleFile(singlePath) =>
            val headerBytes = readPhysicalBytes(singlePath)
            val header =
              ByteBuffer
                .wrap(headerBytes)
                .order(ByteOrder.LITTLE_ENDIAN)
            (
              singlePath,
              singlePath,
              header.getFloat(108).toInt
            )
          case NiftiFiles.PairFile(headerPath, imagePath) =>
            (headerPath, imagePath, 0)

      val headerBytes = readPhysicalBytes(headerPath)
      val header =
        ByteBuffer
          .wrap(headerBytes)
          .order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(header.getShort(70).toInt, datatype.code)
      assertEquals(
        header.getShort(72).toInt,
        datatype.bitsPerValue
      )
      assertEquals(header.getFloat(112), 1.0f)
      assertEquals(header.getFloat(116), 0.0f)

      val payload = readPhysicalBytes(payloadPath)
      val expected =
        encodedBytes(datatype, Vector(0.0, 1.0, 2.0))
      assertEquals(
        payload
          .slice(payloadOffset, payloadOffset + expected.length)
          .toVector,
        expected
      )

      val decoded = niftiRight(Nifti.readScaledDouble(path))
      assertEquals(decoded.header.datatype, datatype)
      decoded.image.fold(
        _ => fail("NIfTI must produce D3"),
        d3 =>
          Vector(0.0, 1.0, 2.0).zipWithIndex.foreach {
            case (expectedValue, index) =>
              assertEquals(
                imageValue(
                  d3.value.valueAt(Vector(index, 0, 0))
                ),
                expectedValue
              )
          }
      )

  test("scaled integer output records and uses canonical header scaling"):
    val frame = rasFrame("scaled-integer")
    val options =
      writeOptionsRight(
        NiftiWriteOptions.create(
          NiftiDatatype.Int16,
          slope = 0.1,
          intercept = 10.0
        )
      )
    assertEquals(options.slope, 0.1f.toDouble)
    assertEquals(options.intercept, 10.0f.toDouble)
    val values =
      Vector(-1.0, 0.0, 1.0).map { raw =>
        raw * options.slope + options.intercept
      }
    val image = scalarImage(frame, values)
    val path = temporaryPath("scaled-short.nii")

    niftiRight(Nifti.writeScalar(path, image, options))
    val bytes = Files.readAllBytes(path)
    val buffer =
      ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    assertEquals(buffer.getFloat(112), options.slope.toFloat)
    assertEquals(buffer.getFloat(116), options.intercept.toFloat)
    assertEquals(
      Vector.tabulate(3)(index => buffer.getShort(352 + index * 2)),
      Vector(-1.toShort, 0.toShort, 1.toShort)
    )

    val decoded = niftiRight(Nifti.readScaledDouble(path))
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        values.zipWithIndex.foreach { case (expected, index) =>
          assertEqualsDouble(
            imageValue(d3.value.valueAt(Vector(index, 0, 0))),
            expected,
            1e-7
          )
        }
    )

    assertEquals(
      NiftiWriteOptions.create(NiftiDatatype.Int16, 0.0, 0.0),
      Left(NiftiWriteOptionsError.InvalidSlope(0.0))
    )
    assertEquals(
      NiftiWriteOptions.create(
        NiftiDatatype.Int16,
        Double.MinPositiveValue,
        0.0
      ),
      Left(
        NiftiWriteOptionsError.InvalidSlope(
          Double.MinPositiveValue
        )
      )
    )
    assertEquals(
      NiftiWriteOptions.create(
        NiftiDatatype.Int16,
        1.0,
        Double.MaxValue
      ),
      Left(
        NiftiWriteOptionsError.InvalidIntercept(Double.MaxValue)
      )
    )

  test("integer narrowing rejects loss unless quantization is explicit"):
    val frame = rasFrame("integer-policy")
    val fractional = scalarImage(frame, Vector(1.5))
    val exactOptions =
      NiftiWriteOptions.forDatatype(NiftiDatatype.UInt8)
    val headerPath = temporaryPath("fractional.hdr")
    val imagePath = headerPath.resolveSibling("fractional.img")

    assertEquals(
      Nifti.writeScalar(headerPath, fractional, exactOptions),
      Left(
        NiftiError.ValueNotRepresentable(
          Vector(0, 0, 0),
          1.5,
          1.5,
          NiftiDatatype.UInt8,
          NiftiValueProblem.Fractional
        )
      )
    )
    assert(!Files.exists(headerPath))
    assert(!Files.exists(imagePath))

    val quantizedOptions =
      writeOptionsRight(
        NiftiWriteOptions.create(
          NiftiDatatype.UInt8,
          slope = 1.0,
          intercept = 0.0,
          integerConversion =
            NiftiIntegerConversion.RoundToNearestEven
        )
      )
    val quantized =
      scalarImage(frame, Vector(1.5, 2.5, 3.5))
    val quantizedPath = temporaryPath("quantized.nii")
    niftiRight(
      Nifti.writeScalar(
        quantizedPath,
        quantized,
        quantizedOptions
      )
    )
    assertEquals(
      Files
        .readAllBytes(quantizedPath)
        .slice(352, 355)
        .toVector,
      Vector[Byte](2, 2, 4)
    )

  test("integer output checks signed, unsigned, and non-finite bounds"):
    val frame = rasFrame("integer-bounds")
    val cases =
      Vector(
        NiftiDatatype.UInt8 ->
          Vector(0.0, 255.0),
        NiftiDatatype.Int16 ->
          Vector(Short.MinValue.toDouble, Short.MaxValue.toDouble),
        NiftiDatatype.Int32 ->
          Vector(Int.MinValue.toDouble, Int.MaxValue.toDouble)
      )
    cases.foreach { case (datatype, values) =>
      val path =
        temporaryPath(s"${datatype.toString.toLowerCase}-bounds.nii")
      val image = scalarImage(frame, values)
      niftiRight(
        Nifti.writeScalar(
          path,
          image,
          NiftiWriteOptions.forDatatype(datatype)
        )
      )
      val decoded = niftiRight(Nifti.readScaledDouble(path))
      decoded.image.fold(
        _ => fail("NIfTI must produce D3"),
        d3 =>
          values.zipWithIndex.foreach { case (expected, index) =>
            assertEquals(
              imageValue(
                d3.value.valueAt(Vector(index, 0, 0))
              ),
              expected
            )
          }
      )
    }

    val outOfRangePath = temporaryPath("unsigned-out-of-range.nii")
    assertEquals(
      Nifti.writeScalar(
        outOfRangePath,
        scalarImage(frame, Vector(-1.0)),
        NiftiWriteOptions.forDatatype(NiftiDatatype.UInt8)
      ),
      Left(
        NiftiError.ValueNotRepresentable(
          Vector(0, 0, 0),
          -1.0,
          -1.0,
          NiftiDatatype.UInt8,
          NiftiValueProblem.OutsideRange(0.0, 255.0)
        )
      )
    )
    assert(!Files.exists(outOfRangePath))

    val nonFinitePath = temporaryPath("integer-nonfinite.nii")
    Nifti.writeScalar(
      nonFinitePath,
      scalarImage(frame, Vector(Double.NaN)),
      NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
    ) match
      case Left(
            NiftiError.ValueNotRepresentable(
              Vector(0, 0, 0),
              value,
              encoded,
              NiftiDatatype.Int16,
              NiftiValueProblem.NonFinite
            )
          ) =>
        assert(value.isNaN)
        assert(encoded.isNaN)
      case other =>
        fail(s"expected non-finite conversion failure, got $other")
    assert(!Files.exists(nonFinitePath))

  test("Float32 rounds normally but rejects finite overflow"):
    val frame = rasFrame("float32-policy")
    val options =
      NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)
    val roundedPath = temporaryPath("float32-rounded.nii")
    val value = 1.0 / 10.0
    niftiRight(
      Nifti.writeScalar(
        roundedPath,
        scalarImage(frame, Vector(value)),
        options
      )
    )
    val raw =
      ByteBuffer
        .wrap(Files.readAllBytes(roundedPath))
        .order(ByteOrder.LITTLE_ENDIAN)
        .getFloat(352)
    assertEquals(raw, value.toFloat)
    val decoded = niftiRight(Nifti.readScaledDouble(roundedPath))
    decoded.image.fold(
      _ => fail("NIfTI must produce D3"),
      d3 =>
        assertEquals(
          imageValue(d3.value.valueAt(Vector(0, 0, 0))),
          value.toFloat.toDouble
        )
    )

    val overflow = Float.MaxValue.toDouble * 2.0
    val overflowPath = temporaryPath("float32-overflow.nii")
    assertEquals(
      Nifti.writeScalar(
        overflowPath,
        scalarImage(frame, Vector(overflow)),
        options
      ),
      Left(
        NiftiError.ValueNotRepresentable(
          Vector(0, 0, 0),
          overflow,
          overflow,
          NiftiDatatype.Float32,
          NiftiValueProblem.FloatingOverflow
        )
      )
    )
    assert(!Files.exists(overflowPath))

  test("single and pair files preserve multiple extension blocks exactly"):
    val first =
      extensionRight(
        NiftiExtension.create(
          0,
          Vector[Byte](1, 2, 3, 4, 5)
        )
      )
    val second =
      extensionRight(
        NiftiExtension.create(
          42,
          Vector.tabulate[Byte](21)(_.toByte)
        )
      )
    val extensions = Vector(first, second)
    val frame = rasFrame("extension-writer")
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3]))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(1, 1, 1), Vector(11.0))
        )
      )

    val singlePath = temporaryPath("extensions.nii")
    niftiRight(
      Nifti.writeScalar(singlePath, image, extensions = extensions)
    )
    val singleHeader = niftiRight(Nifti.readHeader(singlePath))
    assertEquals(singleHeader.extensions, extensions)
    assertEquals(
      singleHeader.voxelOffset,
      352 + extensions.map(_.encodedSize).sum
    )

    val pairPath = temporaryPath("extensions.hdr")
    niftiRight(
      Nifti.writeScalar(
        pairPath,
        image,
        extensions = singleHeader.extensions
      )
    )
    val pairHeader = niftiRight(Nifti.readHeader(pairPath))
    assertEquals(pairHeader.extensions, extensions)
    assertEquals(pairHeader.voxelOffset, 0)
    assertEquals(
      Files.size(pairPath),
      (352 + extensions.map(_.encodedSize).sum).toLong
    )

    val rewritten = temporaryPath("extensions-rewritten.nii.gz")
    niftiRight(
      Nifti.writeScalar(
        rewritten,
        image,
        extensions = pairHeader.extensions
      )
    )
    val rewrittenHeader = niftiRight(Nifti.readHeader(rewritten))
    assertEquals(
      rewrittenHeader.extensions.map(_.payload),
      extensions.map(_.payload)
    )

  test("malformed extensions return precise typed errors"):
    assertEquals(
      NiftiExtension.create(-1, Vector.empty),
      Left(NiftiExtensionError.InvalidCode(-1))
    )

    val frame = rasFrame("malformed-extension-writer")
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3]))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(1, 1, 1), Vector(1.0))
        )
      )
    val extension =
      extensionRight(NiftiExtension.create(6, Vector[Byte](1, 2, 3)))

    def mutated(name: String, size: Int): Either[NiftiError, NiftiHeader] =
      val path = temporaryPath(name)
      niftiRight(
        Nifti.writeScalar(
          path,
          image,
          extensions = Vector(extension)
        )
      )
      val bytes = Files.readAllBytes(path)
      ByteBuffer
        .wrap(bytes)
        .order(ByteOrder.LITTLE_ENDIAN)
        .putInt(352, size)
      Files.write(path, bytes)
      Nifti.readHeader(path)

    assertEquals(
      mutated("too-small-extension.nii", 8),
      Left(
        NiftiError.Extension(
          NiftiExtensionError.InvalidBlockSize(0, 8)
        )
      )
    )
    assertEquals(
      mutated("misaligned-extension.nii", 20),
      Left(
        NiftiError.Extension(
          NiftiExtensionError.MisalignedBlockSize(0, 20)
        )
      )
    )
    assertEquals(
      mutated("oversized-extension.nii", 64),
      Left(
        NiftiError.Extension(
          NiftiExtensionError.BlockExceedsRegion(0, 64, 16)
        )
      )
    )

  test("malformed paths and missing pair companions return typed errors"):
    val truncated = temporaryPath("truncated.nii")
    Files.write(truncated, Array.fill[Byte](20)(0))
    Nifti.readHeader(truncated) match
      case Left(_: NiftiError.UnexpectedEndOfFile) => ()
      case other => fail(s"expected typed EOF error, got $other")

    val unsupported = temporaryPath("unsupported.nii")
    writeFixture(
      unsupported,
      dimensions = Vector(1, 1, 1),
      datatype = NiftiDatatype.Float64,
      values = Vector(1.0)
    )
    val bytes = Files.readAllBytes(unsupported)
    val header = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    header.putShort(70, 512.toShort)
    Files.write(unsupported, bytes)
    assertEquals(
      Nifti.readHeader(unsupported),
      Left(NiftiError.UnsupportedDatatype(512, 64))
    )

    val frame = geometryRight(Frame.named[D3]("path-errors"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3]))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](1, 1, 1)
        )
      )
    val unsupportedOutput = temporaryPath("output.dat")
    assertEquals(
      Nifti.writeScalar(unsupportedOutput, image),
      Left(NiftiError.UnsupportedPath(unsupportedOutput.toString))
    )

    val mismatched = temporaryPath("mismatched-storage.nii")
    niftiRight(Nifti.writeScalar(mismatched, image))
    val mismatchedBytes = Files.readAllBytes(mismatched)
    mismatchedBytes(345) = 'i'.toByte
    Files.write(mismatched, mismatchedBytes)
    assertEquals(
      Nifti.readHeader(mismatched),
      Left(
        NiftiError.StorageMismatch(
          mismatched.toString,
          NiftiStorage.SingleFile,
          NiftiStorage.PairFile
        )
      )
    )

    val headerPath = temporaryPath("missing-companion.hdr")
    val imagePath = headerPath.resolveSibling("missing-companion.img")
    niftiRight(Nifti.writeScalar(headerPath, image))
    Files.delete(imagePath)
    assertEquals(
      Nifti.readHeader(headerPath),
      Left(
        NiftiError.MissingCompanion(
          headerPath.toString,
          imagePath.toString
        )
      )
    )

  private final case class QForm(
      b: Double,
      c: Double,
      d: Double,
      offset: Vector[Double],
      qfac: Double
  )

  private def writeFixture(
      path: Path,
      dimensions: Vector[Int],
      datatype: NiftiDatatype = NiftiDatatype.Float64,
      order: NiftiByteOrder = NiftiByteOrder.LittleEndian,
      spacing: Vector[Double] = Vector(1.0, 1.0, 1.0),
      slope: Double = 1.0,
      intercept: Double = 0.0,
      qform: Option[QForm] = None,
      sform: Option[Vector[Double]] = None,
      spatialUnitCode: Int = 2,
      values: Vector[Double],
      compressed: Boolean = false
  ): Path =
    val bytesPerValue = datatype.bitsPerValue / 8
    val bytes =
      new Array[Byte](352 + values.length * bytesPerValue)
    val buffer = ByteBuffer.wrap(bytes).order(javaOrder(order))
    buffer.putInt(0, 348)
    buffer.putShort(40, dimensions.length.toShort)
    var axis = 0
    while axis < 7 do
      buffer.putShort(
        42 + axis * 2,
        (if axis < dimensions.length then dimensions(axis) else 1).toShort
      )
      axis += 1
    buffer.putShort(70, datatype.code.toShort)
    buffer.putShort(72, datatype.bitsPerValue.toShort)
    buffer.putFloat(76, qform.fold(1.0)(_.qfac).toFloat)
    axis = 0
    while axis < 3 do
      buffer.putFloat(80 + axis * 4, spacing(axis).toFloat)
      axis += 1
    buffer.putFloat(108, 352.0f)
    buffer.putFloat(112, slope.toFloat)
    buffer.putFloat(116, intercept.toFloat)
    buffer.put(123, spatialUnitCode.toByte)
    qform.foreach { form =>
      buffer.putShort(252, 1.toShort)
      buffer.putFloat(256, form.b.toFloat)
      buffer.putFloat(260, form.c.toFloat)
      buffer.putFloat(264, form.d.toFloat)
      buffer.putFloat(268, form.offset(0).toFloat)
      buffer.putFloat(272, form.offset(1).toFloat)
      buffer.putFloat(276, form.offset(2).toFloat)
      ()
    }
    sform.foreach { affine =>
      buffer.putShort(254, 2.toShort)
      var row = 0
      while row < 3 do
        var column = 0
        while column < 4 do
          buffer.putFloat(
            280 + row * 16 + column * 4,
            affine(row * 4 + column).toFloat
          )
          column += 1
        row += 1
      ()
    }
    buffer.put(344, 'n'.toByte)
    buffer.put(345, '+'.toByte)
    buffer.put(346, '1'.toByte)
    buffer.put(347, 0.toByte)
    values.zipWithIndex.foreach { case (value, index) =>
      val offset = 352 + index * bytesPerValue
      datatype match
        case NiftiDatatype.UInt8 =>
          buffer.put(offset, value.toInt.toByte)
        case NiftiDatatype.Int16 =>
          buffer.putShort(offset, value.toInt.toShort)
        case NiftiDatatype.Int32 =>
          buffer.putInt(offset, value.toInt)
        case NiftiDatatype.Float32 =>
          buffer.putFloat(offset, value.toFloat)
        case NiftiDatatype.Float64 =>
          buffer.putDouble(offset, value)
      ()
    }
    if compressed then
      val output = new GZIPOutputStream(Files.newOutputStream(path))
      try output.write(bytes)
      finally output.close()
      path
    else Files.write(path, bytes)

  private def scalarImage(
      frame: Frame[D3],
      values: Vector[Double]
  ) =
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

  private def readPhysicalBytes(path: Path): Array[Byte] =
    if path.toString.toLowerCase.endsWith(".gz") then
      val input =
        new GZIPInputStream(Files.newInputStream(path))
      try input.readAllBytes()
      finally input.close()
    else Files.readAllBytes(path)

  private def encodedBytes(
      datatype: NiftiDatatype,
      values: Vector[Double]
  ): Vector[Byte] =
    val buffer =
      ByteBuffer
        .allocate(
          values.length * (datatype.bitsPerValue / 8)
        )
        .order(ByteOrder.LITTLE_ENDIAN)
    values.foreach { value =>
      datatype match
        case NiftiDatatype.UInt8 =>
          buffer.put(value.toInt.toByte)
        case NiftiDatatype.Int16 =>
          buffer.putShort(value.toInt.toShort)
        case NiftiDatatype.Int32 =>
          buffer.putInt(value.toInt)
        case NiftiDatatype.Float32 =>
          buffer.putFloat(value.toFloat)
        case NiftiDatatype.Float64 =>
          buffer.putDouble(value)
      ()
    }
    buffer.array().toVector

  private def temporaryPath(name: String): Path =
    Files.createTempDirectory("image4s-nifti-suite").resolve(name)

  private def javaOrder(order: NiftiByteOrder): ByteOrder =
    order match
      case NiftiByteOrder.LittleEndian => ByteOrder.LITTLE_ENDIAN
      case NiftiByteOrder.BigEndian    => ByteOrder.BIG_ENDIAN

  private def assertRows(
      actual: Vector[Double],
      expected: Vector[Double],
      tolerance: Double = 1e-6
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach { case (left, right) =>
      assertEqualsDouble(left, right, tolerance)
    }

  private def imageValue[A](
      value: Either[image4s.ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def rasFrame(label: String): Frame[D3] =
    geometryRight(
      Frame.named[D3](
        label,
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )

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

  private def writeOptionsRight(
      value: Either[NiftiWriteOptionsError, NiftiWriteOptions]
  ): NiftiWriteOptions =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def niftiRight[A](
      value: Either[NiftiError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
