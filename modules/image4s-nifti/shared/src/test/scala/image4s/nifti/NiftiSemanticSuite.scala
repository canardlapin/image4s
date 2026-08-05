package image4s.nifti

import image4s.Axis
import image4s.AxisCoordinate
import image4s.AxisKind
import image4s.AxisUnit
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.SomeSampled
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import ravel.UInt8

import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable

final class NiftiSemanticSuite extends ScalaCheckSuite:
  private val files = new MemoryNiftiFileSystem
  private val api = new NiftiApi[String](files)

  property("malformed header fuzzing always returns a typed result"):
    forAll(
      Gen.listOfN(
        348,
        Gen
          .choose(Byte.MinValue.toInt, Byte.MaxValue.toInt)
          .map(_.toByte)
      )
    ): bytes =>
      val path = "/fuzz-header.nii"
      files.put(path, bytes.toArray)
      api.readHeader(
        path,
        NiftiIoLimits.default.copy(
          maximumPayloadBytes = 1024,
          maximumDecodedBytes = 1024,
          maximumExtensionBytes = 1024
        )
      ) match
        case Left(_: NiftiError) => ()
        case Right(header) =>
          assert(header.dimensions.nonEmpty)
          assert(header.dimensions.forall(_ > 0))

  property("random legal dimensions preserve dtype and logical signatures"):
    forAll(
      Gen.choose(1, 5),
      Gen.choose(1, 5),
      Gen.choose(1, 5),
      Gen.oneOf(NiftiDatatype.values.toVector),
      Gen.oneOf(NiftiByteOrder.values.toVector)
    ): (x, y, z, datatype, order) =>
      val count = x * y * z
      val values =
        Vector.tabulate(count)(index =>
          datatype match
            case NiftiDatatype.UInt8 =>
              (index % 251).toDouble
            case NiftiDatatype.Int16 =>
              ((index % 2001) - 1000).toDouble
            case NiftiDatatype.Int32 =>
              (index * 1009 - 50000).toDouble
            case NiftiDatatype.Float32 =>
              index.toDouble * 0.25 - 10.0
            case NiftiDatatype.Float64 =>
              index.toDouble * 0.125 - 20.0
        )
      val path = s"/generated-$x-$y-$z-$datatype-$order.nii"
      writeFixture(
        path,
        Vector(x, y, z),
        datatype,
        order = order,
        values = values
      )
      val decoded = niftiRight(api.readRaw(path))
      val actual = rawAsDoubles(decoded.image)
      val expectedLogical =
        (for
          first <- 0 until x
          second <- 0 until y
          third <- 0 until z
        yield values(
          first + x * (second + y * third)
        )).toVector
      assertEquals(decoded.header.dimensions, Vector(x, y, z))
      assertEquals(decoded.header.byteOrder, order)
      assertEquals(decoded.header.datatype, datatype)
      assertEquals(actual, expectedLogical)
      assertEquals(
        actual.zipWithIndex.map { case (value, index) =>
          value * (index.toDouble + 1.0)
        }.sum,
        expectedLogical.zipWithIndex.map { case (value, index) =>
          value * (index.toDouble + 1.0)
        }.sum
      )

  test("raw and scaled reads preserve every supported dtype in both byte orders"):
    val cases =
      Vector(
        NiftiDatatype.UInt8 -> Vector(0.0, 17.0, 255.0),
        NiftiDatatype.Int16 -> Vector(-32768.0, 0.0, 32767.0),
        NiftiDatatype.Int32 ->
          Vector(Int.MinValue.toDouble, 0.0, Int.MaxValue.toDouble),
        NiftiDatatype.Float32 -> Vector(-2.5, 0.0, 1.25),
        NiftiDatatype.Float64 -> Vector(-2.25, 0.125, 9.5)
      )
    val scalings =
      Vector(
        0.0 -> 5.0,
        2.0 -> 3.0,
        -0.5 -> 1.0
      )

    for
      order <- NiftiByteOrder.values.toVector
      (datatype, rawValues) <- cases
      ((slope, intercept), scalingIndex) <- scalings.zipWithIndex
    do
      val path =
        s"/matrix-$order-$datatype-$scalingIndex.nii"
      writeFixture(
        path,
        dimensions = Vector(3, 1, 1),
        datatype = datatype,
        order = order,
        slope = slope,
        intercept = intercept,
        values = rawValues
      )

      val raw = niftiRight(api.readRaw(path))
      assertEquals(raw.image.datatype, datatype)
      assertEquals(rawAsDoubles(raw.image), rawValues)
      assertEquals(
        rawAsDoubles(raw.image).sum,
        rawValues.sum
      )

      val scaled = niftiRight(api.readScaledDouble(path))
      val expectedScaled =
        if slope == 0.0 then rawValues
        else rawValues.map(_ * slope + intercept)
      assertEquals(
        sampledValues(scaled.image),
        expectedScaled
      )
      assertEquals(
        sampledValues(scaled.image).sum,
        expectedScaled.sum
      )

      val rounded =
        niftiRight(
          api.readScaledFloat(
            path,
            NiftiFloatPrecision.AllowRounding
          )
        )
      assertEquals(
        sampledValues(rounded.image),
        expectedScaled.map(_.toFloat)
      )

  test("converted payloads preserve 4D order across chunk boundaries"):
    val dimensions = Vector(3, 2, 1, 4)
    val rawValues = Vector.tabulate(dimensions.product)(_.toDouble - 8.0)
    val options =
      NiftiReadOptions.default.copy(
        ioLimits = NiftiIoLimits.default.copy(workingBufferBytes = 12)
      )
    val expectedRawOrder =
      (for
        x <- 0 until dimensions(0)
        y <- 0 until dimensions(1)
        z <- 0 until dimensions(2)
        t <- 0 until dimensions(3)
      yield rawValues(
        x + dimensions(0) *
          (y + dimensions(1) * (z + dimensions(2) * t))
      )).toVector

    NiftiByteOrder.values.foreach: order =>
      val scalarPath = s"/converted-4d-$order.nii"
      writeFixture(
        scalarPath,
        dimensions,
        NiftiDatatype.Float32,
        order = order,
        slope = 2.0,
        intercept = 0.5,
        values = rawValues
      )
      val expectedScaled = expectedRawOrder.map(_ * 2.0 + 0.5)
      val doubles = niftiRight(api.readScaledDouble(scalarPath, options))
      val floats = niftiRight(api.readScaledFloat(scalarPath, options = options))

      assertEquals(sampledValues(doubles.image), expectedScaled)
      assertEquals(sampledValues(floats.image), expectedScaled.map(_.toFloat))

      val labelsPath = s"/converted-labels-4d-$order.nii"
      writeFixture(
        labelsPath,
        dimensions,
        NiftiDatatype.Int16,
        order = order,
        slope = 2.0,
        intercept = 1.0,
        values = rawValues
      )
      val labels = niftiRight(api.readLabels(labelsPath, options))
      assertEquals(
        sampledValues(labels.image),
        expectedRawOrder.map(value => (value * 2.0 + 1.0).toLong)
      )

  test("scalar stored and scalar double surfaces retain explicit interpretation"):
    val path = "/scalar-stored-uint8.nii"
    writeFixture(
      path,
      dimensions = Vector(3, 1, 1),
      datatype = NiftiDatatype.UInt8,
      slope = 2.0,
      intercept = 1.0,
      values = Vector(0.0, 17.0, 255.0)
    )

    val stored = niftiRight(api.readScalarStored(path))
    stored.image match
      case NiftiScalarStored.UInt8(codes, encoding) =>
        assertEquals(
          codes.value.data.elementsIterator.map(_.toInt).toList,
          List(0, 17, 255)
        )
        assertEquals(
          encoding.decode(UInt8.unsafe(255), Vector.empty).map(_.toInt),
          Right(255)
        )
      case other =>
        fail(s"expected UInt8 stored scalar, got $other")

    val scalar = niftiRight(api.readScalar(path))
    assertEquals(sampledValues(scalar.image), Vector(1.0, 35.0, 511.0))
    val float = niftiRight(
      api.readScalarAs(
        path,
        NiftiValueConversion.ScaledFloat(
          NiftiFloatPrecision.RejectLossy
        )
      )
    )
    assertEquals(sampledValues(float.image), Vector(1.0f, 35.0f, 511.0f))

  test("native labels retain integer codes and reject scaled or floating input"):
    val cases =
      Vector(
        NiftiDatatype.UInt8 -> Vector(0.0, 128.0, 255.0),
        NiftiDatatype.Int16 -> Vector(-32768.0, 0.0, 32767.0),
        NiftiDatatype.Int32 ->
          Vector(Int.MinValue.toDouble, 0.0, Int.MaxValue.toDouble)
      )

    cases.zipWithIndex.foreach { case ((datatype, values), index) =>
      val path = s"/native-labels-$index.nii"
      writeFixture(
        path,
        dimensions = Vector(3, 1, 1),
        datatype = datatype,
        slope = 0.0,
        intercept = 99.0,
        values = values
      )
      val labels = niftiRight(api.readLabelsNative(path))
      assertEquals(labels.image.datatype, datatype)
      assertEquals(nativeLabelCodes(labels.image), values.map(_.toLong))
    }

    writeFixture(
      "/native-labels-scaled.nii",
      dimensions = Vector(1, 1, 1),
      datatype = NiftiDatatype.Int16,
      slope = 2.0,
      intercept = 1.0,
      values = Vector(3.0)
    )
    assertEquals(
      api.readLabelsNative("/native-labels-scaled.nii"),
      Left(NiftiError.NativeLabelRequiresIdentityScale(2.0, 1.0))
    )

    writeFixture(
      "/native-labels-float.nii",
      dimensions = Vector(1, 1, 1),
      datatype = NiftiDatatype.Float32,
      values = Vector(7.0)
    )
    assertEquals(
      api.readLabelsNative("/native-labels-float.nii"),
      Left(NiftiError.NativeLabelDatatypeMustBeIntegral(NiftiDatatype.Float32))
    )

  test("Float conversion makes rounding, overflow, and readAs policy explicit"):
    val exactPath = "/float32-exact.nii"
    writeFixture(
      exactPath,
      Vector(2, 1, 1),
      NiftiDatatype.Float32,
      values = Vector(1.25, -2.5)
    )
    val exact = niftiRight(api.readScaledFloat(exactPath))
    assertEquals(sampledValues(exact.image), Vector(1.25f, -2.5f))

    val lossyPath = "/float64-lossy.nii"
    writeFixture(
      lossyPath,
      Vector(1, 1, 1),
      NiftiDatatype.Float64,
      values = Vector(0.1)
    )
    api.readScaledFloat(lossyPath) match
      case Left(
            NiftiError.ReadValueNotRepresentable(
              Vector(0, 0, 0),
              0.1,
              0.1,
              NiftiDatatype.Float64,
              "Float",
              NiftiReadValueProblem.PrecisionLoss
            )
          ) =>
        ()
      case other =>
        fail(s"expected explicit Float precision loss, got $other")

    val rounded =
      niftiRight(
        api.readAs(
          lossyPath,
          NiftiValueConversion.ScaledFloat(
            NiftiFloatPrecision.AllowRounding
          )
        )
      )
    assertEquals(sampledValues(rounded.image), Vector(0.1f))

    val integerLossPath = "/int32-float-loss.nii"
    writeFixture(
      integerLossPath,
      Vector(1, 1, 1),
      NiftiDatatype.Int32,
      values = Vector(16777217.0)
    )
    assert(
      api.readScaledFloat(integerLossPath).left.exists {
        case NiftiError.ReadValueNotRepresentable(
              _,
              _,
              _,
              _,
              _,
              NiftiReadValueProblem.PrecisionLoss
            ) =>
          true
        case _ =>
          false
      }
    )

    val overflowPath = "/float64-overflow.nii"
    writeFixture(
      overflowPath,
      Vector(1, 1, 1),
      NiftiDatatype.Float64,
      values = Vector(Float.MaxValue.toDouble * 2.0)
    )
    assert(
      api
        .readScaledFloat(
          overflowPath,
          NiftiFloatPrecision.AllowRounding
        )
        .left
        .exists {
          case NiftiError.ReadValueNotRepresentable(
                _,
                _,
                _,
                _,
                _,
                NiftiReadValueProblem.FloatingOverflow
              ) =>
            true
          case _ =>
            false
        }
    )

  test("label reads preserve exact integral categories and reject fractional scaling"):
    val exactCases =
      Vector(
        NiftiDatatype.UInt8 -> Vector(0.0, 1.0, 7.0),
        NiftiDatatype.Int16 -> Vector(-2.0, 0.0, 9.0),
        NiftiDatatype.Int32 -> Vector(-100000.0, 0.0, 100000.0),
        NiftiDatatype.Float32 -> Vector(-3.0, 0.0, 11.0),
        NiftiDatatype.Float64 -> Vector(-5.0, 0.0, 13.0)
      )
    exactCases.foreach { case (datatype, raw) =>
      val path = s"/labels-$datatype.nii"
      writeFixture(
        path,
        Vector(3, 1, 1),
        datatype,
        slope = 2.0,
        intercept = 1.0,
        values = raw
      )
      val decoded = niftiRight(api.readLabels(path))
      assertEquals(
        sampledValues(decoded.image),
        raw.map(value => (value * 2.0 + 1.0).toLong)
      )
    }

    val fractionalPath = "/fractional-labels.nii"
    writeFixture(
      fractionalPath,
      Vector(1, 1, 1),
      NiftiDatatype.Int16,
      slope = 0.5,
      intercept = 0.0,
      values = Vector(1.0)
    )
    assertEquals(
      api.readLabels(fractionalPath),
      Left(
        NiftiError.ReadValueNotRepresentable(
          Vector(0, 0, 0),
          1.0,
          0.5,
          NiftiDatatype.Int16,
          "exact Long label",
          NiftiReadValueProblem.FractionalLabel
        )
      )
    )

    val nonFinitePath = "/nonfinite-labels.nii"
    writeFixture(
      nonFinitePath,
      Vector(1, 1, 1),
      NiftiDatatype.Float64,
      values = Vector(Double.NaN)
    )
    api.readLabels(nonFinitePath) match
      case Left(
            NiftiError.ReadValueNotRepresentable(
              _,
              raw,
              scaled,
              NiftiDatatype.Float64,
              _,
              NiftiReadValueProblem.NonFiniteLabel
            )
          ) =>
        assert(raw.isNaN)
        assert(scaled.isNaN)
      case other =>
        fail(s"expected non-finite label rejection, got $other")

    val longBoundaryPath = "/long-label-boundaries.nii"
    writeFixture(
      longBoundaryPath,
      Vector(1, 1, 1),
      NiftiDatatype.Float64,
      values = Vector(Long.MinValue.toDouble)
    )
    assertEquals(
      sampledValues(niftiRight(api.readLabels(longBoundaryPath)).image),
      Vector(Long.MinValue)
    )

    val longOverflowPath = "/long-label-overflow.nii"
    val longUpperExclusive = 9223372036854775808.0
    writeFixture(
      longOverflowPath,
      Vector(1, 1, 1),
      NiftiDatatype.Float64,
      values = Vector(longUpperExclusive)
    )
    assert(
      api.readLabels(longOverflowPath).left.exists {
        case NiftiError.ReadValueNotRepresentable(
              _,
              _,
              _,
              _,
              _,
              NiftiReadValueProblem.LabelOutsideLongRange
            ) =>
          true
        case _ =>
          false
      }
    )

  test("exact Long labels round trip only through integral storage"):
    val frame =
      geometryRight(
        Frame.named[D3](
          "labels",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(3, 1, 1), Affine.identity[D3])
      )
    val labels =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(
            Shape(3, 1, 1),
            Vector(-32768L, 0L, 32767L)
          )
        )
      )
    val path = "/label-roundtrip.nii"

    niftiRight(
      api.writeLabels(
        path,
        labels,
        NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
      )
    )
    assertEquals(
      sampledValues(niftiRight(api.readLabels(path)).image),
      Vector(-32768L, 0L, 32767L)
    )
    assertEquals(
      nativeLabelCodes(niftiRight(api.readLabelsNative(path)).image),
      Vector(-32768L, 0L, 32767L)
    )
    assertEquals(
      api.writeLabels(
        "/invalid-label-dtype.nii",
        labels,
        NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)
      ),
      Left(
        NiftiError.LabelDatatypeMustBeIntegral(
          NiftiDatatype.Float32
        )
      )
    )
    val quantizing =
      NiftiWriteOptions
        .create(
          NiftiDatatype.Int16,
          slope = 1.0,
          intercept = 0.0,
          integerConversion = NiftiIntegerConversion.RoundToNearestEven
        )
        .fold(error => fail(error.message), identity)
    assertEquals(
      api.writeLabels("/quantized-labels.nii", labels, quantizing),
      Left(NiftiError.LabelWriteRequiresExactIntegerConversion)
    )

  test("fourth-axis sampling recovers time units and applies unknown-unit policy"):
    val cases =
      Vector(
        8 -> AxisUnit.Seconds,
        16 -> AxisUnit.Milliseconds,
        24 -> AxisUnit.Microseconds
      )
    cases.foreach { case (temporalCode, expectedUnit) =>
      val path = s"/time-$temporalCode.nii"
      writeFixture(
        path,
        Vector(1, 1, 1, 3),
        NiftiDatatype.Float32,
        pixelDimensions = Vector(1.0, 1.0, 1.0, 0.8),
        temporalUnitCode = temporalCode,
        values = Vector(1.0, 2.0, 3.0)
      )
      val axis = soleNonSpatialAxis(niftiRight(api.readScaledDouble(path)).image)
      assertEquals(axis.kind, AxisKind.Time)
      assertEquals(
        imageRight(axis.coordinateAt(2)),
        AxisCoordinate.Numeric(1.6f.toDouble, expectedUnit)
      )
    }

    val unknownPath = "/time-unknown.nii"
    writeFixture(
      unknownPath,
      Vector(1, 1, 1, 2),
      NiftiDatatype.Float32,
      pixelDimensions = Vector(1.0, 1.0, 1.0, 2.5),
      temporalUnitCode = 0,
      values = Vector(1.0, 2.0)
    )
    val ordinal =
      soleNonSpatialAxis(
        niftiRight(api.readScaledDouble(unknownPath)).image
      )
    assertEquals(ordinal.kind, AxisKind.Other)
    assertEquals(
      imageRight(ordinal.coordinateAt(1)),
      AxisCoordinate.Ordinal(1)
    )

    assertEquals(
      api.readScaledDouble(
        unknownPath,
        NiftiReadOptions.default.copy(
          unknownTemporalUnit = NiftiUnknownTemporalUnitPolicy.Reject
        )
      ),
      Left(NiftiError.UnknownTemporalUnitForFourthDimension)
    )

    val assumed =
      soleNonSpatialAxis(
        niftiRight(
          api.readScaledDouble(
            unknownPath,
            NiftiReadOptions.default.copy(
              unknownTemporalUnit = NiftiUnknownTemporalUnitPolicy.AssumeMilliseconds
            )
          )
        ).image
      )
    assertEquals(assumed.kind, AxisKind.Time)
    assertEquals(
      imageRight(assumed.coordinateAt(1)),
      AxisCoordinate.Numeric(2.5, AxisUnit.Milliseconds)
    )

  test("affine selection covers preference, agreement, diagnostics, fallback, and override"):
    val identity = Affine.identity[D3]
    val shifted =
      affine(
        Vector(
          1.0, 0.0, 0.0, 10.0, 0.0, 1.0, 0.0, 20.0, 0.0, 0.0, 1.0, 30.0, 0.0, 0.0, 0.0, 1.0
        )
      )
    val explicit =
      affine(
        Vector(
          1.0, 0.0, 0.0, 99.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0
        )
      )

    val qformOnly = "/qform-only.nii"
    writeFixture(
      qformOnly,
      Vector(1, 1, 1),
      NiftiDatatype.Float32,
      qformOffset = Some(Vector(1.0, 2.0, 3.0)),
      values = Vector(1.0)
    )
    assertEquals(
      niftiRight(api.readScaledDouble(qformOnly)).affineSelection.source,
      NiftiAffineSource.Qform
    )

    val sformOnly = "/sform-only.nii"
    writeFixture(
      sformOnly,
      Vector(1, 1, 1),
      NiftiDatatype.Float32,
      sform = Some(shifted.rowMajor),
      values = Vector(1.0)
    )
    assertEquals(
      niftiRight(api.readScaledDouble(sformOnly)).affineSelection.source,
      NiftiAffineSource.Sform
    )

    val agreement = "/forms-agree.nii"
    writeFixture(
      agreement,
      Vector(1, 1, 1),
      NiftiDatatype.Float32,
      qformOffset = Some(Vector(0.0, 0.0, 0.0)),
      sform = Some(identity.rowMajor),
      values = Vector(1.0)
    )
    val agreed =
      niftiRight(
        api.readScaledDouble(
          agreement,
          NiftiReadOptions.default.copy(
            affinePolicy = NiftiAffinePolicy.RequireAgreement(0.0)
          )
        )
      )
    assertEquals(agreed.affineSelection.source, NiftiAffineSource.Sform)
    assertEquals(agreed.affineSelection.diagnostics, Vector.empty)

    val disagreement = "/forms-disagree.nii"
    writeFixture(
      disagreement,
      Vector(1, 1, 1),
      NiftiDatatype.Float32,
      qformOffset = Some(Vector(0.0, 0.0, 0.0)),
      sform = Some(shifted.rowMajor),
      values = Vector(1.0)
    )
    val preferSform =
      niftiRight(api.readScaledDouble(disagreement))
    assertEquals(
      preferSform.affineSelection.source,
      NiftiAffineSource.Sform
    )
    assertEquals(
      preferSform.affineSelection.diagnostics,
      Vector(NiftiDiagnostic.QformSformDisagreement(30.0))
    )
    assert(preferSform.header.qform.nonEmpty)
    assert(preferSform.header.sform.nonEmpty)

    val preferQform =
      niftiRight(
        api.readScaledDouble(
          disagreement,
          NiftiReadOptions.default.copy(
            affinePolicy = NiftiAffinePolicy.PreferQform
          )
        )
      )
    assertEquals(
      preferQform.affineSelection.source,
      NiftiAffineSource.Qform
    )

    assertEquals(
      api.readScaledDouble(
        disagreement,
        NiftiReadOptions.default.copy(
          affinePolicy = NiftiAffinePolicy.RequireAgreement(1e-6)
        )
      ),
      Left(NiftiError.AffineFormsDisagree(30.0, 1e-6))
    )
    api.readScaledDouble(
      disagreement,
      NiftiReadOptions.default.copy(
        affinePolicy = NiftiAffinePolicy.RequireAgreement(Double.NaN)
      )
    ) match
      case Left(NiftiError.InvalidAffineAgreementTolerance(value)) =>
        assert(value.isNaN)
      case other =>
        fail(s"expected invalid NaN affine tolerance, got $other")

    val overridden =
      niftiRight(
        api.readScaledDouble(
          disagreement,
          NiftiReadOptions.default.copy(
            affinePolicy = NiftiAffinePolicy.UseExplicit(explicit)
          )
        )
      )
    assertEquals(
      overridden.affineSelection.source,
      NiftiAffineSource.Explicit
    )
    assertEquals(
      overridden.affineSelection.affine.rowMajor,
      explicit.rowMajor
    )
    assertEquals(
      overridden.affineSelection.diagnostics,
      preferSform.affineSelection.diagnostics
    )

    val fallback = "/affine-fallback.nii"
    writeFixture(
      fallback,
      Vector(1, 1, 1),
      NiftiDatatype.Float32,
      values = Vector(1.0)
    )
    assertEquals(
      niftiRight(api.readScaledDouble(fallback)).affineSelection.source,
      NiftiAffineSource.Fallback
    )

  private def sampledValues[A, Sem](
      sampled: SomeSampled[A, Sem]
  ): Vector[A] =
    sampled.fold(
      _ => fail("NIfTI semantic court expects D3"),
      d3 => d3.value.data.elementsIterator.toVector
    )

  private def soleNonSpatialAxis[A, Sem](
      sampled: SomeSampled[A, Sem]
  ): Axis =
    sampled.fold(
      _ => fail("NIfTI semantic court expects D3"),
      d3 =>
        assertEquals(d3.value.nonSpatialAxes.size, 1)
        d3.value.nonSpatialAxes(0).getOrElse(fail("missing axis"))
    )

  private def rawAsDoubles(raw: NiftiRawImage): Vector[Double] =
    raw match
      case NiftiRawImage.UInt8(image) =>
        sampledValues(image).map(_.toInt.toDouble)
      case NiftiRawImage.Int16(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Int32(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Float32(image) =>
        sampledValues(image).map(_.toDouble)
      case NiftiRawImage.Float64(image) =>
        sampledValues(image)

  private def nativeLabelCodes(
      labels: NiftiLabelStored
  ): Vector[Long] =
    labels match
      case NiftiLabelStored.UInt8(codes, _) =>
        sampledValues(codes).map(_.toInt.toLong)
      case NiftiLabelStored.Int16(codes, _) =>
        sampledValues(codes).map(_.toLong)
      case NiftiLabelStored.Int32(codes, _) =>
        sampledValues(codes).map(_.toLong)

  private def writeFixture(
      path: String,
      dimensions: Vector[Int],
      datatype: NiftiDatatype,
      order: NiftiByteOrder = NiftiByteOrder.LittleEndian,
      pixelDimensions: Vector[Double] = Vector(1.0, 1.0, 1.0),
      slope: Double = 1.0,
      intercept: Double = 0.0,
      temporalUnitCode: Int = 0,
      qformOffset: Option[Vector[Double]] = None,
      sform: Option[Vector[Double]] = None,
      values: Vector[Double]
  ): Unit =
    val bytesPerValue = datatype.bitsPerValue / 8
    val bytes =
      new Array[Byte](352 + values.length * bytesPerValue)
    val buffer =
      ByteBuffer.wrap(bytes).order(javaOrder(order))
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
    buffer.putFloat(76, 1.0f)
    axis = 0
    while axis < dimensions.length do
      buffer.putFloat(
        80 + axis * 4,
        pixelDimensions.lift(axis).getOrElse(1.0).toFloat
      )
      axis += 1
    buffer.putFloat(108, 352.0f)
    buffer.putFloat(112, slope.toFloat)
    buffer.putFloat(116, intercept.toFloat)
    buffer.put(123, (2 | temporalUnitCode).toByte)
    qformOffset.foreach { offset =>
      buffer.putShort(252, 1.toShort)
      buffer.putFloat(268, offset(0).toFloat)
      buffer.putFloat(272, offset(1).toFloat)
      buffer.putFloat(276, offset(2).toFloat)
    }
    sform.foreach { values =>
      buffer.putShort(254, 1.toShort)
      var row = 0
      while row < 3 do
        var column = 0
        while column < 4 do
          buffer.putFloat(
            280 + row * 16 + column * 4,
            values(row * 4 + column).toFloat
          )
          column += 1
        row += 1
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
    }
    files.put(path, bytes)

  private def affine(values: Vector[Double]): Affine[D3] =
    geometryRight(Affine.fromRowMajor[D3](values))

  private def javaOrder(order: NiftiByteOrder): ByteOrder =
    order match
      case NiftiByteOrder.LittleEndian => ByteOrder.LITTLE_ENDIAN
      case NiftiByteOrder.BigEndian => ByteOrder.BIG_ENDIAN

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](
      value: Either[image4s.ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def niftiRight[A](
      value: Either[NiftiError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

private final class MemoryNiftiFileSystem extends NiftiFileSystem[String]:
  private val bytesByPath =
    mutable.Map.empty[String, Array[Byte]]

  def put(path: String, bytes: Array[Byte]): Unit =
    bytesByPath.update(path, bytes.clone())

  def show(path: String): String =
    path

  def fileName(path: String): String =
    path.split('/').lastOption.getOrElse(path)

  def sibling(path: String, fileName: String): String =
    path.lastIndexOf('/') match
      case -1 =>
        fileName
      case index =>
        path.substring(0, index + 1) + fileName

  def exists(path: String): Boolean =
    bytesByPath.contains(path)

  def ioStrategy(_path: String): NiftiIoStrategy =
    NiftiIoStrategy.BoundedStreaming

  def readBytes(
      path: String,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]] =
    bytesByPath
      .get(path)
      .map(bytes => Right(bytes.clone()))
      .getOrElse(
        Left(NiftiError.IoFailure(path, operation, "missing"))
      )

  def writeBytes(
      path: String,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit] =
    put(path, bytes)
    Right(())
