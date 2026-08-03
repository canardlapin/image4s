package image4s.nifti

import image4s.Axis
import image4s.AxisKind
import image4s.AxisUnit
import image4s.Categorical
import image4s.CategoricalImage
import image4s.Continuous
import image4s.ContinuousImage
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.SomeSampled
import image4s.ValueSemantics
import ravel.AnyRank
import ravel.DType
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import ravel.{UInt8 as RavelUInt8}
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import image4s.geometry.LengthUnit

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets

private[nifti] final class NiftiApi[P](
    fileSystem: NiftiFileSystem[P]
):
  def ioStrategy(path: P): NiftiIoStrategy =
    fileSystem.ioStrategy(path)

  def readHeader(
      path: P,
      limits: NiftiIoLimits = NiftiIoLimits.default
  ): Either[NiftiError, NiftiHeader] =
    for
      _ <- validateIoLimits(limits)
      files <- resolveForRead(path)
      header <- readResolvedHeader(files, limits)
    yield header

  def readRaw(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[NiftiRawImage]
  ] =
    for
      header <- readHeader(path, options.ioLimits)
      frame <- freshFrame(path, header, options)
      selection <- selectAffine(header, options.affinePolicy)
      decoded <- readRawInPrepared(path, frame, header, selection, options)
    yield DecodedNifti(decoded, header, selection)

  /** Read native storage codes with their structural storage interpretation.
    *
    * No NIfTI slope/intercept scaling is applied. The header retained by
    * [[DecodedNifti]] remains the receipt for that affine interpretation.
    */
  def readScalarStored(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[NiftiScalarStored]
  ] =
    readRaw(path, options).map { decoded =>
      DecodedNifti(
        NiftiScalarStored.fromRaw(decoded.image),
        decoded.header,
        decoded.affineSelection
      )
    }

  /** Read scaled scalar values at caller-selected floating precision. */
  def readScalarAs[A](
      path: P,
      conversion: NiftiValueConversion[A],
      options: NiftiReadOptions = NiftiReadOptions.default
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[A, Continuous]]
  ] =
    readAs(path, conversion, options)

  /** Double-precision convenience spelling for [[readScalarAs]]. */
  def readScalar(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[Double, Continuous]]
  ] =
    readScalarAs(path, NiftiValueConversion.ScaledDouble, options)

  def readAs[A](
      path: P,
      conversion: NiftiValueConversion[A],
      options: NiftiReadOptions = NiftiReadOptions.default
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[A, Continuous]]
  ] =
    for
      header <- readHeader(path, options.ioLimits)
      frame <- freshFrame(path, header, options)
      selection <- selectAffine(header, options.affinePolicy)
      decoded <- readIn[A, Continuous](
        path,
        frame,
        header,
        selection,
        options,
        convertScaled(conversion, header)
      )
    yield DecodedNifti(SomeSampled.d3(decoded), header, selection)

  def readScaledFloat(
      path: P,
      precision: NiftiFloatPrecision = NiftiFloatPrecision.RejectLossy,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[Float, Continuous]]
  ] =
    readAs(
      path,
      NiftiValueConversion.ScaledFloat(precision),
      options
    )

  def readScaledDouble(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[Double, Continuous]]
  ] =
    readAs(path, NiftiValueConversion.ScaledDouble, options)

  def readLabels(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[Long, Categorical]]
  ] =
    for
      header <- readHeader(path, options.ioLimits)
      frame <- freshFrame(path, header, options)
      selection <- selectAffine(header, options.affinePolicy)
      decoded <- readIn[Long, Categorical](
        path,
        frame,
        header,
        selection,
        options,
        convertLabel(header)
      )
    yield DecodedNifti(SomeSampled.d3(decoded), header, selection)

  /** Read categorical codes without widening their native integer storage.
    *
    * This strict surface accepts only UInt8, Int16, and Int32 inputs whose
    * header applies no effective scaling. In accordance with the NIfTI
    * convention, a zero slope means "do not scale" and is accepted regardless
    * of its intercept.
    */
  def readLabelsNative(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[NiftiLabelStored]
  ] =
    for
      header <- readHeader(path, options.ioLimits)
      _ <- validateNativeLabelHeader(header)
      frame <- freshFrame(path, header, options)
      selection <- selectAffine(header, options.affinePolicy)
      raw <- readRawInPrepared(path, frame, header, selection, options)
      labels <- NiftiLabelStored.fromRaw(raw)
    yield DecodedNifti(labels, header, selection)

  def readRawIn[F <: Frame[D3]](
      path: P,
      frame: F,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[NiftiError, DecodedNifti[NiftiRawImage]] =
    for
      header <- readHeader(path, options.ioLimits)
      _ <- validateSuppliedFrame(frame, header)
      selection <- selectAffine(header, options.affinePolicy)
      decoded <- readRawInPrepared(path, frame, header, selection, options)
    yield DecodedNifti(decoded, header, selection)

  def readAsIn[A, F <: Frame[D3]](
      path: P,
      frame: F,
      conversion: NiftiValueConversion[A],
      options: NiftiReadOptions = NiftiReadOptions.default
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    NiftiError,
    DecodedNifti[
      ContinuousImage[
        ? <: SampleSpace[frame.type, D3],
        A,
        AnyRank
      ]
    ]
  ] =
    for
      header <- readHeader(path, options.ioLimits)
      _ <- validateSuppliedFrame(frame, header)
      selection <- selectAffine(header, options.affinePolicy)
      decoded <- readIn[A, Continuous](
        path,
        frame,
        header,
        selection,
        options,
        convertScaled(conversion, header)
      )
    yield DecodedNifti(decoded, header, selection)

  def readScaledDoubleIn[F <: Frame[D3]](
      path: P,
      frame: F,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[
      ContinuousImage[
        ? <: SampleSpace[frame.type, D3],
        Double,
        AnyRank
      ]
    ]
  ] =
    readAsIn(
      path,
      frame,
      NiftiValueConversion.ScaledDouble,
      options
    )

  def readScaledFloatIn[F <: Frame[D3]](
      path: P,
      frame: F,
      precision: NiftiFloatPrecision = NiftiFloatPrecision.RejectLossy,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[
      ContinuousImage[
        ? <: SampleSpace[frame.type, D3],
        Float,
        AnyRank
      ]
    ]
  ] =
    readAsIn(
      path,
      frame,
      NiftiValueConversion.ScaledFloat(precision),
      options
    )

  def readLabelsIn[F <: Frame[D3]](
      path: P,
      frame: F,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[
      CategoricalImage[
        ? <: SampleSpace[frame.type, D3],
        Long,
        AnyRank
      ]
    ]
  ] =
    for
      header <- readHeader(path, options.ioLimits)
      _ <- validateSuppliedFrame(frame, header)
      selection <- selectAffine(header, options.affinePolicy)
      decoded <- readIn[Long, Categorical](
        path,
        frame,
        header,
        selection,
        options,
        convertLabel(header)
      )
    yield DecodedNifti(decoded, header, selection)

  def writeScalar[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      R <: AnyRank
  ](
      path: P,
      image: ContinuousImage[S, Double, R],
      options: NiftiWriteOptions = NiftiWriteOptions.default,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[P]] =
    writeValues(path, image, options, extensions, WriteValueSource.double)

  def writeLabels[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      R <: AnyRank
  ](
      path: P,
      image: CategoricalImage[S, Long, R],
      options: NiftiWriteOptions =
        NiftiWriteOptions.forDatatype(NiftiDatatype.Int32),
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[P]] =
    if
      options.integerConversion !=
        NiftiIntegerConversion.RejectLossy
    then
      Left(NiftiError.LabelWriteRequiresExactIntegerConversion)
    else
      options.datatype match
        case NiftiDatatype.UInt8 |
            NiftiDatatype.Int16 |
            NiftiDatatype.Int32 =>
          writeValues(path, image, options, extensions, WriteValueSource.long)
        case datatype =>
          Left(NiftiError.LabelDatatypeMustBeIntegral(datatype))

  private def readRawInPrepared[
      F <: Frame[D3]
  ](
      path: P,
      frame: F,
      header: NiftiHeader,
      selection: NiftiAffineSelection,
      options: NiftiReadOptions
  ): Either[NiftiError, NiftiRawImage] =
    header.datatype match
      case NiftiDatatype.UInt8 =>
        readNativeIn[RavelUInt8, NiftiRaw](
          path,
          frame,
          header,
          selection,
          options,
          NativePayloadReader.uint8
        ).map(image =>
          NiftiRawImage.UInt8(SomeSampled.d3(image))
        )
      case NiftiDatatype.Int16 =>
        readNativeIn[Short, NiftiRaw](
          path,
          frame,
          header,
          selection,
          options,
          NativePayloadReader.short
        ).map(image =>
          NiftiRawImage.Int16(SomeSampled.d3(image))
        )
      case NiftiDatatype.Int32 =>
        readNativeIn[Int, NiftiRaw](
          path,
          frame,
          header,
          selection,
          options,
          NativePayloadReader.int
        ).map(image =>
          NiftiRawImage.Int32(SomeSampled.d3(image))
        )
      case NiftiDatatype.Float32 =>
        readNativeIn[Float, NiftiRaw](
          path,
          frame,
          header,
          selection,
          options,
          NativePayloadReader.float
        ).map(image =>
          NiftiRawImage.Float32(SomeSampled.d3(image))
        )
      case NiftiDatatype.Float64 =>
        readNativeIn[Double, NiftiRaw](
          path,
          frame,
          header,
          selection,
          options,
          NativePayloadReader.double
        ).map(image =>
          NiftiRawImage.Float64(SomeSampled.d3(image))
        )

  private def readNativeIn[
      A,
      Sem
  ](
      path: P,
      frame: Frame[D3],
      header: NiftiHeader,
      selection: NiftiAffineSelection,
      options: NiftiReadOptions,
      reader: NativePayloadReader[A]
  )(using DType[A], ValueSemantics[A, Sem]): Either[
    NiftiError,
    Sampled[
      ? <: SampleSpace[frame.type, D3],
      A,
      Sem,
      AnyRank
    ]
  ] =
    for
      axes <- axesFrom(header, options)
      shape <- Shape
        .from(header.logicalShape)
        .left
        .map(error => NiftiError.InvalidArrayShape(error.toString))
      logicalData <-
        readPayloadNative(path, header, shape, reader, options.ioLimits)
      grid <- Grid
        .in(frame)(header.spatialShape, selection.affine)
        .left
        .map(NiftiError.Geometry.apply)
      sampled <- Sampled
        .create[frame.type, D3, A, Sem, AnyRank](
          grid,
          axes,
          logicalData
        )
        .left
        .map(NiftiError.Image.apply)
    yield sampled

  /** Typed native payload writers keep primitive storage on the hot decode path.
    *
    * The reader loop is shared, but each writer is compiled with a concrete
    * Ravel dtype. Passing a generic `(ByteBuffer, Int) => A` decoder here would
    * route opaque unsigned and primitive values through the generic builder
    * path on some JVMs, allocating one boxed value per sample.
    */
  private trait NativePayloadReader[A]:
    def bytesPerValue: Int

    def write(
        builder: ravel.ArrayBuilder[A],
        logicalOffset: Int,
        buffer: ByteBuffer,
        byteOffset: Int
    ): Unit

    def read(
        payloadPath: P,
        header: NiftiHeader,
        shape: Shape[AnyRank],
        limits: NiftiIoLimits
    )(using DType[A]): Either[NiftiError, NDArray[A, AnyRank]] =
      val payloadBytes = shape.size.toLong * bytesPerValue.toLong
      val workingBytes =
        alignedWorkingBuffer(limits.workingBufferBytes, bytesPerValue)
      val dimensions = header.logicalShape
      val indices = new Array[Int](dimensions.length)
      var result: Either[NiftiError, Unit] = Right(())
      val data =
        NDArray.build[A, AnyRank](shape) { builder =>
          var fileValueOffset = 0
          result = fileSystem.readChunks(
            payloadPath,
            NiftiOperation.ReadPayload,
            header.voxelOffset.toLong,
            payloadBytes,
            workingBytes
          ) { (bytes, length) =>
            val buffer =
              ByteBuffer
                .wrap(bytes, 0, length)
                .order(javaOrder(header.byteOrder))
            var byteOffset = 0
            while byteOffset < length do
              decodeFirstAxisFastest(
                fileValueOffset,
                dimensions,
                indices
              )
              val logicalOffset =
                lastAxisFastestOffset(indices, dimensions)
              write(builder, logicalOffset, buffer, byteOffset)
              fileValueOffset += 1
              byteOffset += bytesPerValue
            Right(())
          }
        }
      result.map(_ => data)

  private object NativePayloadReader:
    val uint8: NativePayloadReader[RavelUInt8] =
      new NativePayloadReader[RavelUInt8]:
        val bytesPerValue = 1

        def write(
            builder: ravel.ArrayBuilder[RavelUInt8],
            logicalOffset: Int,
            buffer: ByteBuffer,
            byteOffset: Int
        ): Unit =
          builder.writeLinear(
            logicalOffset,
            RavelUInt8.unsafe(buffer.get(byteOffset) & 0xff)
          )

    val short: NativePayloadReader[Short] =
      new NativePayloadReader[Short]:
        val bytesPerValue = 2

        def write(
            builder: ravel.ArrayBuilder[Short],
            logicalOffset: Int,
            buffer: ByteBuffer,
            byteOffset: Int
        ): Unit =
          builder.writeLinear(logicalOffset, buffer.getShort(byteOffset))

    val int: NativePayloadReader[Int] =
      new NativePayloadReader[Int]:
        val bytesPerValue = 4

        def write(
            builder: ravel.ArrayBuilder[Int],
            logicalOffset: Int,
            buffer: ByteBuffer,
            byteOffset: Int
        ): Unit =
          builder.writeLinear(logicalOffset, buffer.getInt(byteOffset))

    val float: NativePayloadReader[Float] =
      new NativePayloadReader[Float]:
        val bytesPerValue = 4

        def write(
            builder: ravel.ArrayBuilder[Float],
            logicalOffset: Int,
            buffer: ByteBuffer,
            byteOffset: Int
        ): Unit =
          builder.writeLinear(logicalOffset, buffer.getFloat(byteOffset))

    val double: NativePayloadReader[Double] =
      new NativePayloadReader[Double]:
        val bytesPerValue = 8

        def write(
            builder: ravel.ArrayBuilder[Double],
            logicalOffset: Int,
            buffer: ByteBuffer,
            byteOffset: Int
        ): Unit =
          builder.writeLinear(logicalOffset, buffer.getDouble(byteOffset))

  private def readIn[
      A,
      Sem
  ](
      path: P,
      frame: Frame[D3],
      header: NiftiHeader,
      selection: NiftiAffineSelection,
      options: NiftiReadOptions,
      convert: (Double, Array[Int]) => A
  )(using DType[A], ValueSemantics[A, Sem]): Either[
    NiftiError,
    Sampled[
      ? <: SampleSpace[frame.type, D3],
      A,
      Sem,
      AnyRank
    ]
  ] =
    for
      axes <- axesFrom(header, options)
      shape <- Shape
        .from(header.logicalShape)
        .left
        .map(error => NiftiError.InvalidArrayShape(error.toString))
      logicalData <-
        readPayloadAs(path, header, shape, convert, options.ioLimits)
      grid <- Grid
        .in(frame)(header.spatialShape, selection.affine)
        .left
        .map(NiftiError.Geometry.apply)
      sampled <- Sampled
        .create[frame.type, D3, A, Sem, AnyRank](
          grid,
          axes,
          logicalData
        )
        .left
        .map(NiftiError.Image.apply)
    yield sampled

  private def axesFrom(
      header: NiftiHeader,
      options: NiftiReadOptions
  ): Either[NiftiError, NonSpatialAxes] =
    val created =
      header.nonSpatialShape.zipWithIndex.foldLeft[
        Either[NiftiError, Vector[Axis]]
      ](Right(Vector.empty)) { case (accumulated, (extent, index)) =>
        for
          axes <- accumulated
          axis <-
            if index == 0 then
              fourthAxis(header, extent, options)
            else ordinalAxis(index, extent)
        yield axes :+ axis
      }
    created.flatMap { axes =>
      NonSpatialAxes
        .from(axes)
        .left
        .map(NiftiError.Image.apply)
    }

  private def fourthAxis(
      header: NiftiHeader,
      extent: Int,
      options: NiftiReadOptions
  ): Either[NiftiError, Axis] =
    val step = header.pixelDimensions.lift(3).getOrElse(1.0)
    header.temporalUnit match
      case NiftiTemporalUnit.Second =>
        regularTimeAxis(extent, step, AxisUnit.Seconds)
      case NiftiTemporalUnit.Millisecond =>
        regularTimeAxis(extent, step, AxisUnit.Milliseconds)
      case NiftiTemporalUnit.Microsecond =>
        regularTimeAxis(extent, step, AxisUnit.Microseconds)
      case NiftiTemporalUnit.Hertz =>
        regularFrequencyAxis(extent, step, AxisUnit.Hertz)
      case NiftiTemporalUnit.Ppm =>
        regularFrequencyAxis(extent, step, AxisUnit.PartsPerMillion)
      case NiftiTemporalUnit.RadianPerSecond =>
        regularFrequencyAxis(extent, step, AxisUnit.RadiansPerSecond)
      case NiftiTemporalUnit.Unknown =>
        options.unknownTemporalUnit match
          case NiftiUnknownTemporalUnitPolicy.Ordinal =>
            ordinalAxis(0, extent)
          case NiftiUnknownTemporalUnitPolicy.Reject =>
            Left(NiftiError.UnknownTemporalUnitForFourthDimension)
          case NiftiUnknownTemporalUnitPolicy.AssumeSeconds =>
            regularTimeAxis(extent, step, AxisUnit.Seconds)
          case NiftiUnknownTemporalUnitPolicy.AssumeMilliseconds =>
            regularTimeAxis(extent, step, AxisUnit.Milliseconds)
          case NiftiUnknownTemporalUnitPolicy.AssumeMicroseconds =>
            regularTimeAxis(extent, step, AxisUnit.Microseconds)

  private def regularTimeAxis(
      extent: Int,
      step: Double,
      unit: AxisUnit
  ): Either[NiftiError, Axis] =
    Axis
      .regular("time", AxisKind.Time, extent, 0.0, step, unit)
      .left
      .map(NiftiError.Image.apply)

  private def regularFrequencyAxis(
      extent: Int,
      step: Double,
      unit: AxisUnit
  ): Either[NiftiError, Axis] =
    for
      kind <- AxisKind
        .custom("frequency")
        .left
        .map(NiftiError.Image.apply)
      axis <- Axis
        .regular("frequency", kind, extent, 0.0, step, unit)
        .left
        .map(NiftiError.Image.apply)
    yield axis

  private def ordinalAxis(
      index: Int,
      extent: Int
  ): Either[NiftiError, Axis] =
    Axis
      .create(
        s"nifti-axis-${index + 4}",
        extent,
        AxisKind.Other
      )
      .left
      .map(NiftiError.Image.apply)

  private def freshFrame(
      path: P,
      header: NiftiHeader,
      options: NiftiReadOptions
  ): Either[NiftiError, Frame[D3]] =
    val label =
      Option(fileSystem.fileName(path))
        .filter(_.nonEmpty)
        .getOrElse("nifti")
    Frame
      .named[D3](
        label,
        geometryUnit(header.spatialUnit, options.fallbackSpatialUnit),
        CoordinateConvention.RAS
      )
      .left
      .map(NiftiError.Geometry.apply)

  private def validateSuppliedFrame(
      frame: Frame[D3],
      header: NiftiHeader
  ): Either[NiftiError, Unit] =
    if frame.convention != CoordinateConvention.RAS then
      Left(
        NiftiError.FrameConventionMismatch(frame.convention)
      )
    else
      header.spatialUnit match
        case NiftiSpatialUnit.Unknown =>
          Right(())
        case unit
            if geometryUnit(unit, frame.unit) != frame.unit =>
          Left(
            NiftiError.FrameUnitMismatch(unit, frame.unit)
          )
        case _ =>
          Right(())

  private def selectAffine(
      header: NiftiHeader,
      policy: NiftiAffinePolicy
  ): Either[NiftiError, NiftiAffineSelection] =
    val difference =
      for
        qform <- header.qform
        sform <- header.sform
      yield maxAbsoluteDifference(qform, sform)
    val diagnostics =
      difference
        .filter(_ > 0.0)
        .map(value =>
          Vector(
            NiftiDiagnostic.QformSformDisagreement(value)
          )
        )
        .getOrElse(Vector.empty)

    def selected(
        affine: Affine[D3],
        source: NiftiAffineSource
    ): Either[NiftiError, NiftiAffineSelection] =
      Right(NiftiAffineSelection(affine, source, diagnostics))

    def fallback: Either[NiftiError, NiftiAffineSelection] =
      selected(header.fallbackAffine, NiftiAffineSource.Fallback)

    policy match
      case NiftiAffinePolicy.PreferSform =>
        header.sform match
          case Some(affine) =>
            selected(affine, NiftiAffineSource.Sform)
          case None =>
            header.qform match
              case Some(affine) =>
                selected(affine, NiftiAffineSource.Qform)
              case None =>
                fallback
      case NiftiAffinePolicy.PreferQform =>
        header.qform match
          case Some(affine) =>
            selected(affine, NiftiAffineSource.Qform)
          case None =>
            header.sform match
              case Some(affine) =>
                selected(affine, NiftiAffineSource.Sform)
              case None =>
                fallback
      case NiftiAffinePolicy.RequireAgreement(tolerance) =>
        if !tolerance.isFinite || tolerance < 0.0 then
          Left(
            NiftiError.InvalidAffineAgreementTolerance(tolerance)
          )
        else
          (header.qform, header.sform, difference) match
            case (Some(_), Some(_), Some(value))
                if value > tolerance =>
              Left(
                NiftiError.AffineFormsDisagree(value, tolerance)
              )
            case (_, Some(affine), _) =>
              selected(affine, NiftiAffineSource.Sform)
            case (Some(affine), None, _) =>
              selected(affine, NiftiAffineSource.Qform)
            case (None, None, _) =>
              fallback
      case NiftiAffinePolicy.UseExplicit(affine) =>
        selected(affine, NiftiAffineSource.Explicit)

  private def maxAbsoluteDifference(
      left: Affine[D3],
      right: Affine[D3]
  ): Double =
    left.rowMajor
      .zip(right.rowMajor)
      .foldLeft(0.0) { case (largest, (a, b)) =>
        math.max(largest, math.abs(a - b))
      }

  private def convertScaled[A](
      conversion: NiftiValueConversion[A],
      header: NiftiHeader
  ): (Double, Array[Int]) => A =
    conversion match
      case NiftiValueConversion.ScaledDouble =>
        (raw, _) => scaledValue(raw, header)
      case NiftiValueConversion.ScaledFloat(precision) =>
        (raw, logicalIndex) =>
          val scaled = scaledValue(raw, header)
          val narrowed = scaled.toFloat
          if scaled.isFinite && !narrowed.isFinite then
            throw ReadConversionFailure(
              readValueError(
                logicalIndex,
                raw,
                scaled,
                header.datatype,
                "Float",
                NiftiReadValueProblem.FloatingOverflow
              )
            )
          else if
            precision == NiftiFloatPrecision.RejectLossy &&
            !sameFloatingValue(scaled, narrowed.toDouble)
          then
            throw ReadConversionFailure(
              readValueError(
                logicalIndex,
                raw,
                scaled,
                header.datatype,
                "Float",
                NiftiReadValueProblem.PrecisionLoss
              )
            )
          else narrowed

  private def convertLabel(
      header: NiftiHeader
  ): (Double, Array[Int]) => Long =
    (raw, logicalIndex) =>
      val scaled = scaledValue(raw, header)
      if !scaled.isFinite then
        throw ReadConversionFailure(
          readValueError(
            logicalIndex,
            raw,
            scaled,
            header.datatype,
            "exact Long label",
            NiftiReadValueProblem.NonFiniteLabel
          )
        )
      else if scaled != math.rint(scaled) then
        throw ReadConversionFailure(
          readValueError(
            logicalIndex,
            raw,
            scaled,
            header.datatype,
            "exact Long label",
            NiftiReadValueProblem.FractionalLabel
          )
        )
      else if
        scaled < Long.MinValue.toDouble ||
        scaled >= LongUpperExclusive
      then
        throw ReadConversionFailure(
          readValueError(
            logicalIndex,
            raw,
            scaled,
            header.datatype,
            "exact Long label",
            NiftiReadValueProblem.LabelOutsideLongRange
          )
        )
      else scaled.toLong

  private def validateNativeLabelHeader(
      header: NiftiHeader
  ): Either[NiftiError, Unit] =
    header.datatype match
      case NiftiDatatype.UInt8 | NiftiDatatype.Int16 | NiftiDatatype.Int32 =>
        if header.slope == 0.0 ||
            (header.slope == 1.0 && header.intercept == 0.0)
        then Right(())
        else
          Left(
            NiftiError.NativeLabelRequiresIdentityScale(
              header.slope,
              header.intercept
            )
          )
      case datatype =>
        Left(NiftiError.NativeLabelDatatypeMustBeIntegral(datatype))

  private def readValueError(
      logicalIndex: Array[Int],
      raw: Double,
      scaled: Double,
      datatype: NiftiDatatype,
      target: String,
      problem: NiftiReadValueProblem
  ): NiftiError =
    NiftiError.ReadValueNotRepresentable(
      logicalIndex.toVector,
      raw,
      scaled,
      datatype,
      target,
      problem
    )

  private def scaledValue(
      raw: Double,
      header: NiftiHeader
  ): Double =
    if header.slope == 0.0 then raw
    else raw * header.slope + header.intercept

  private def sameFloatingValue(
      left: Double,
      right: Double
  ): Boolean =
    (left.isNaN && right.isNaN) ||
      java.lang.Double.doubleToRawLongBits(left) ==
        java.lang.Double.doubleToRawLongBits(right)

  private def readPayloadAs[A](
      path: P,
      header: NiftiHeader,
      shape: Shape[AnyRank],
      convert: (Double, Array[Int]) => A,
      limits: NiftiIoLimits
  )(using dtype: DType[A]): Either[NiftiError, NDArray[A, AnyRank]] =
    for
      files <- resolveForRead(path)
      _ <- validatePayloadResources(
        shape,
        header.datatype,
        dtype,
        limits = limits
      )
      data <- decodePayloadAs(
        files.payloadPath,
        header,
        shape,
        convert,
        limits
      )
    yield data

  private def readPayloadNative[A](
      path: P,
      header: NiftiHeader,
      shape: Shape[AnyRank],
      reader: NativePayloadReader[A],
      limits: NiftiIoLimits
  )(using dtype: DType[A]): Either[NiftiError, NDArray[A, AnyRank]] =
    for
      files <- resolveForRead(path)
      _ <- validatePayloadResources(
        shape,
        header.datatype,
        dtype,
        limits = limits
      )
      data <- reader.read(
        files.payloadPath,
        header,
        shape,
        limits
      )
    yield data

  private def decodePayloadAs[A](
      payloadPath: P,
      header: NiftiHeader,
      shape: Shape[AnyRank],
      convert: (Double, Array[Int]) => A,
      limits: NiftiIoLimits
  )(using DType[A]): Either[NiftiError, NDArray[A, AnyRank]] =
    val bytesPerValue = header.datatype.bitsPerValue / 8
    val payloadBytes = shape.size.toLong * bytesPerValue.toLong
    val workingBytes =
      alignedWorkingBuffer(limits.workingBufferBytes, bytesPerValue)
    val dimensions = header.logicalShape
    val indices = new Array[Int](dimensions.length)
    var result: Either[NiftiError, Unit] = Right(())
    val data =
      NDArray.build[A, AnyRank](shape) { builder =>
        var fileValueOffset = 0
        result =
          fileSystem.readChunks(
            payloadPath,
            NiftiOperation.ReadPayload,
            header.voxelOffset.toLong,
            payloadBytes,
            workingBytes
          ) { (bytes, length) =>
            val buffer =
              ByteBuffer
                .wrap(bytes, 0, length)
                .order(javaOrder(header.byteOrder))
            var byteOffset = 0
            var failure: Option[NiftiError] = None
            while byteOffset < length && failure.isEmpty do
              decodeFirstAxisFastest(
                fileValueOffset,
                dimensions,
                indices
              )
              val logicalOffset =
                lastAxisFastestOffset(indices, dimensions)
              val raw =
                readRawValue(buffer, byteOffset, header.datatype)
              try
                builder.writeLinear(
                  logicalOffset,
                  convert(raw, indices)
                )
                fileValueOffset += 1
                byteOffset += bytesPerValue
              catch
                case conversion: ReadConversionFailure =>
                  failure = Some(conversion.error)
            failure.toLeft(())
          }
      }
    result.map(_ => data)

  private def readResolvedHeader(
      files: ResolvedFiles,
      limits: NiftiIoLimits
  ): Either[NiftiError, NiftiHeader] =
    for
      headerBytes <- readExactBytes(
        files.headerPath,
        NiftiOperation.ReadHeader,
        startOffset = 0L,
        byteCount = HeaderSize,
        limits.workingBufferBytes
      )
      header <- parseHeader(
        headerBytes,
        files.storage,
        files.headerPath
      )
      extensionRegion <-
        header.storage match
          case NiftiStorage.SingleFile =>
            val count = header.voxelOffset.toLong - HeaderSize.toLong
            if count > limits.maximumExtensionBytes.toLong then
              Left(
                NiftiError.ExtensionResourceLimitExceeded(
                  count,
                  limits.maximumExtensionBytes
                )
              )
            else
              readExactBytes(
                files.headerPath,
                NiftiOperation.ReadHeader,
                HeaderSize.toLong,
                count.toInt,
                limits.workingBufferBytes
              )
          case NiftiStorage.PairFile =>
            val maximumTotal =
              HeaderSize.toLong + limits.maximumExtensionBytes.toLong
            if maximumTotal >= Int.MaxValue.toLong then
              Left(
                NiftiError.InvalidIoLimit(
                  "maximumExtensionBytes",
                  limits.maximumExtensionBytes.toLong
                )
              )
            else
              fileSystem
                .readUpTo(
                  files.headerPath,
                  NiftiOperation.ReadHeader,
                  maximumTotal.toInt + 1
                )
                .flatMap { bounded =>
                  if bounded.bytes.length.toLong > maximumTotal then
                    Left(
                      NiftiError.ExtensionResourceLimitExceeded(
                        bounded.bytes.length.toLong - HeaderSize.toLong,
                        limits.maximumExtensionBytes
                      )
                    )
                  else
                    Right(
                      bounded.bytes.drop(HeaderSize)
                    )
                }
      extensions <- parseExtensions(
        extensionRegion,
        header.byteOrder
      )
    yield header.copy(extensions = extensions)

  private def parseExtensions(
      bytes: Array[Byte],
      order: NiftiByteOrder
  ): Either[NiftiError, Vector[NiftiExtension]] =
    if bytes.isEmpty then
      Right(Vector.empty)
    else if bytes.length < ExtensionFlagSize then
      Left(
        NiftiError.InvalidHeader(
          NiftiHeaderField.ExtensionFlag,
          s"expected $ExtensionFlagSize bytes, got ${bytes.length}"
        )
      )
    else if bytes(0) == 0.toByte then
      Right(Vector.empty)
    else
      val buffer = ByteBuffer.wrap(bytes).order(javaOrder(order))
      var offset = ExtensionFlagSize
      var index = 0
      var extensions = Vector.empty[NiftiExtension]
      var failure: Option[NiftiError] = None
      while offset < bytes.length && failure.isEmpty do
        val remaining = bytes.length - offset
        if remaining < ExtensionHeaderSize then
          failure =
            Some(
              NiftiError.Extension(
                NiftiExtensionError.BlockExceedsRegion(
                  index,
                  ExtensionHeaderSize,
                  remaining
                )
              )
            )
        else
          val size = buffer.getInt(offset)
          val code = buffer.getInt(offset + 4)
          if size < MinimumExtensionSize then
            failure =
              Some(
                NiftiError.Extension(
                  NiftiExtensionError.InvalidBlockSize(index, size)
                )
              )
          else if size % ExtensionAlignment != 0 then
            failure =
              Some(
                NiftiError.Extension(
                  NiftiExtensionError.MisalignedBlockSize(index, size)
                )
              )
          else if size > remaining then
            failure =
              Some(
                NiftiError.Extension(
                  NiftiExtensionError.BlockExceedsRegion(
                    index,
                    size,
                    remaining
                  )
                )
              )
          else
            val payload =
              bytes
                .slice(
                  offset + ExtensionHeaderSize,
                  offset + size
                )
                .toVector
            NiftiExtension.decoded(code, payload) match
              case Left(error) =>
                failure = Some(NiftiError.Extension(error))
              case Right(extension) =>
                extensions :+= extension
                offset += size
                index += 1
      failure.toLeft(extensions)

  private def parseHeader(
      bytes: Array[Byte],
      expectedStorage: NiftiStorage,
      path: P
  ): Either[NiftiError, NiftiHeader] =
    for
      order <- byteOrder(bytes)
      buffer = ByteBuffer.wrap(bytes).order(javaOrder(order))
      actualStorage <- storage(bytes)
      _ <-
        Either.cond(
          actualStorage == expectedStorage,
          (),
          NiftiError.StorageMismatch(
            fileSystem.show(path),
            expectedStorage,
            actualStorage
          )
        )
      dimensions <- dimensions(buffer)
      datatype <- NiftiDatatype.from(
        unsignedShort(buffer, 70),
        unsignedShort(buffer, 72)
      )
      pixelDimensions <- pixelDimensions(buffer, dimensions.length)
      voxelOffset <- voxelOffset(buffer, actualStorage)
      scaling <- scaling(buffer)
      qform <- qform(buffer, pixelDimensions)
      sform <- sform(buffer)
      fallback <- fallbackAffine(pixelDimensions)
    yield NiftiHeader(
      dimensions = dimensions,
      pixelDimensions = pixelDimensions,
      datatype = datatype,
      voxelOffset = voxelOffset,
      slope = scaling._1,
      intercept = scaling._2,
      qformCode = unsignedShort(buffer, 252),
      qform = qform,
      sformCode = unsignedShort(buffer, 254),
      sform = sform,
      fallbackAffine = fallback,
      byteOrder = order,
      spatialUnit = spatialUnit(bytes(123) & 0x07),
      temporalUnit = temporalUnit(bytes(123) & 0x38),
      storage = actualStorage,
      extensions = Vector.empty
    )

  private def byteOrder(
      bytes: Array[Byte]
  ): Either[NiftiError, NiftiByteOrder] =
    val little =
      ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getInt(0)
    val big =
      ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).getInt(0)
    if little == HeaderSize then Right(NiftiByteOrder.LittleEndian)
    else if big == HeaderSize then Right(NiftiByteOrder.BigEndian)
    else
      Left(
        NiftiError.InvalidHeader(
          NiftiHeaderField.HeaderSize,
          s"expected $HeaderSize in either byte order, got little=$little big=$big"
        )
      )

  private def storage(
      bytes: Array[Byte]
  ): Either[NiftiError, NiftiStorage] =
    val magic =
      new String(bytes, 344, 3, StandardCharsets.US_ASCII)
    (magic, bytes(347)) match
      case ("n+1", 0) =>
        Right(NiftiStorage.SingleFile)
      case ("ni1", 0) =>
        Right(NiftiStorage.PairFile)
      case _ =>
        Left(
          NiftiError.InvalidHeader(
            NiftiHeaderField.Magic,
            s"expected null-terminated n+1 or ni1, got '$magic' with terminator ${bytes(347)}"
          )
        )

  private def dimensions(
      buffer: ByteBuffer
  ): Either[NiftiError, Vector[Int]] =
    val rank = unsignedShort(buffer, 40)
    if rank < 1 || rank > 7 then
      Left(
        NiftiError.InvalidHeader(
          NiftiHeaderField.DimensionCount,
          s"expected a rank from 1 through 7, got $rank"
        )
      )
    else
      val values =
        Vector.tabulate(rank)(axis =>
          unsignedShort(buffer, 42 + axis * 2)
        )
      values.zipWithIndex.collectFirst {
        case (extent, axis) if extent <= 0 =>
          NiftiError.InvalidHeader(
            NiftiHeaderField.Dimension(axis + 1),
            s"expected a positive extent, got $extent"
          )
      }.toLeft(values)

  private def pixelDimensions(
      buffer: ByteBuffer,
      rank: Int
  ): Either[NiftiError, Vector[Double]] =
    val values =
      Vector.tabulate(rank) { axis =>
        math.abs(buffer.getFloat(80 + axis * 4).toDouble)
      }
    values.zipWithIndex.collectFirst {
      case (value, axis) if !value.isFinite =>
        NiftiError.InvalidHeader(
          NiftiHeaderField.PixelDimension(axis + 1),
          s"expected a finite value, got $value"
        )
    } match
      case Some(error) => Left(error)
      case None =>
        Right(values.map(value => if value == 0.0 then 1.0 else value))

  private def voxelOffset(
      buffer: ByteBuffer,
      storage: NiftiStorage
  ): Either[NiftiError, Int] =
    val value = buffer.getFloat(108).toDouble
    val minimum =
      storage match
        case NiftiStorage.SingleFile => DataOffset
        case NiftiStorage.PairFile   => 0
    if value.isFinite &&
      value >= minimum.toDouble &&
      value == math.rint(value) &&
      value <= Int.MaxValue.toDouble
    then Right(value.toInt)
    else
      Left(
        NiftiError.InvalidHeader(
          NiftiHeaderField.VoxelOffset,
          s"expected a finite offset of at least $minimum for $storage, got $value"
        )
      )

  private def scaling(
      buffer: ByteBuffer
  ): Either[NiftiError, (Double, Double)] =
    val slope = buffer.getFloat(112).toDouble
    val intercept = buffer.getFloat(116).toDouble
    if slope.isFinite && intercept.isFinite then
      Right(slope -> intercept)
    else
      Left(
        NiftiError.InvalidHeader(
          NiftiHeaderField.Scaling,
          s"expected finite slope and intercept, got $slope and $intercept"
        )
      )

  private def qform(
      buffer: ByteBuffer,
      pixelDimensions: Vector[Double]
  ): Either[NiftiError, Option[Affine[D3]]] =
    val code = unsignedShort(buffer, 252)
    if code == 0 then Right(None)
    else
      val b = buffer.getFloat(256).toDouble
      val c = buffer.getFloat(260).toDouble
      val d = buffer.getFloat(264).toDouble
      val offset =
        Vector(
          buffer.getFloat(268).toDouble,
          buffer.getFloat(272).toDouble,
          buffer.getFloat(276).toDouble
        )
      val rawQfac = buffer.getFloat(76).toDouble
      val qfac = if rawQfac < 0.0 then -1.0 else 1.0
      quaternionAffine(
        b,
        c,
        d,
        offset,
        pixelDimensions.padTo(3, 1.0).take(3),
        qfac
      ).map(Some.apply)

  private def sform(
      buffer: ByteBuffer
  ): Either[NiftiError, Option[Affine[D3]]] =
    val code = unsignedShort(buffer, 254)
    if code == 0 then Right(None)
    else
      val values =
        Vector.tabulate(16) { flat =>
          val row = flat / 4
          val column = flat % 4
          if row < 3 then
            buffer.getFloat(280 + row * 16 + column * 4).toDouble
          else if column == 3 then 1.0
          else 0.0
        }
      Affine
        .fromRowMajor[D3](values)
        .left
        .map(NiftiError.Geometry.apply)
        .map(Some.apply)

  private def fallbackAffine(
      pixelDimensions: Vector[Double]
  ): Either[NiftiError, Affine[D3]] =
    Affine
      .fromOriginSpacingDirection[D3](
        Vector(0.0, 0.0, 0.0),
        pixelDimensions.padTo(3, 1.0).take(3),
        Vector(
          1.0, 0.0, 0.0,
          0.0, 1.0, 0.0,
          0.0, 0.0, 1.0
        )
      )
      .left
      .map(NiftiError.Geometry.apply)

  private def quaternionAffine(
      b0: Double,
      c0: Double,
      d0: Double,
      offset: Vector[Double],
      spacing: Vector[Double],
      qfac: Double
  ): Either[NiftiError, Affine[D3]] =
    val squaredVector = b0 * b0 + c0 * c0 + d0 * d0
    val (a, b, c, d) =
      if squaredVector > 1.0 - 1e-7 then
        val scale = 1.0 / math.sqrt(squaredVector)
        (0.0, b0 * scale, c0 * scale, d0 * scale)
      else
        (math.sqrt(1.0 - squaredVector), b0, c0, d0)

    val values =
      Vector(
        (a * a + b * b - c * c - d * d) * spacing(0),
        2.0 * (b * c - a * d) * spacing(1),
        2.0 * (b * d + a * c) * spacing(2) * qfac,
        offset(0),
        2.0 * (b * c + a * d) * spacing(0),
        (a * a + c * c - b * b - d * d) * spacing(1),
        2.0 * (c * d - a * b) * spacing(2) * qfac,
        offset(1),
        2.0 * (b * d - a * c) * spacing(0),
        2.0 * (c * d + a * b) * spacing(1),
        (a * a + d * d - c * c - b * b) * spacing(2) * qfac,
        offset(2),
        0.0,
        0.0,
        0.0,
        1.0
      )
    Affine
      .fromRowMajor[D3](values)
      .left
      .map(NiftiError.Geometry.apply)

  private def writeValues[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      A,
      Sem,
      R <: AnyRank
  ](
      path: P,
      image: Sampled[S, A, Sem, R],
      options: NiftiWriteOptions,
      extensions: Vector[NiftiExtension],
      source: WriteValueSource[A]
  ): Either[NiftiError, NiftiFiles[P]] =
    for
      files <- resolveFiles(path)
      result <- writeResolved(
        files,
        image,
        options,
        extensions,
        source
      )
    yield result

  private def writeResolved[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      A,
      Sem,
      R <: AnyRank
  ](
      files: ResolvedFiles,
      image: Sampled[S, A, Sem, R],
      options: NiftiWriteOptions,
      extensions: Vector[NiftiExtension],
      source: WriteValueSource[A]
  ): Either[NiftiError, NiftiFiles[P]] =
    val dimensions = image.logicalShape
    if dimensions.length < 3 || dimensions.length > 7 then
      Left(NiftiError.UnsupportedWriteRank(dimensions.length))
    else if
      options.nonSpatialPixelDimensions.size >
        dimensions.length - 3
    then
      Left(
        NiftiError.NonSpatialPixelDimensionCount(
          options.nonSpatialPixelDimensions.size,
          dimensions.length - 3
        )
      )
    else
      val extensionBytes =
        ExtensionFlagSize.toLong +
          extensions.foldLeft(0L)(_ + _.encodedSize.toLong)
      val headerBytes = HeaderSize.toLong + extensionBytes
      val payloadBytes =
        image.data.size.toLong *
          (options.datatype.bitsPerValue / 8).toLong
      val limits = options.ioLimits
      for
        _ <- validateIoLimits(limits)
        _ <-
          if extensionBytes > limits.maximumExtensionBytes.toLong then
            Left(
              NiftiError.ExtensionResourceLimitExceeded(
                extensionBytes,
                limits.maximumExtensionBytes
              )
            )
          else Right(())
        _ <-
          if payloadBytes > limits.maximumPayloadBytes then
            Left(
              NiftiError.PayloadResourceLimitExceeded(
                NiftiOperation.Write,
                payloadBytes,
                limits.maximumPayloadBytes
              )
            )
          else Right(())
        _ <-
          if headerBytes > Int.MaxValue.toLong then
            Left(NiftiError.OutputTooLarge(headerBytes))
          else Right(())
        header = new Array[Byte](headerBytes.toInt)
        headerBuffer =
          ByteBuffer
            .wrap(header)
            .order(ByteOrder.LITTLE_ENDIAN)
        _ = writeHeader(
          headerBuffer,
          dimensions,
          image,
          files.storage,
          if files.storage == NiftiStorage.SingleFile then
            headerBytes.toInt
          else 0,
          options
        )
        _ = writeExtensions(headerBuffer, extensions)
        _ <- validateFileOrderedValues(
          dimensions,
          image,
          options,
          source
        )
        workingBytes = alignedWorkingBuffer(
          limits.workingBufferBytes,
          options.datatype.bitsPerValue / 8
        )
        written <-
          files.storage match
            case NiftiStorage.SingleFile =>
              fileSystem
                .writeChunks(
                  files.headerPath,
                  header,
                  payloadBytes,
                  workingBytes
                ) { (payloadOffset, bytes, length) =>
                  writeFileOrderedChunk(
                    bytes,
                    length,
                    payloadOffset,
                    dimensions,
                    image,
                    options,
                    source
                  )
                }
                .map(_ => NiftiFiles.SingleFile(files.headerPath))
            case NiftiStorage.PairFile =>
              for
                _ <- fileSystem.writeChunks(
                  files.payloadPath,
                  Array.emptyByteArray,
                  payloadBytes,
                  workingBytes
                ) { (payloadOffset, bytes, length) =>
                  writeFileOrderedChunk(
                    bytes,
                    length,
                    payloadOffset,
                    dimensions,
                    image,
                    options,
                    source
                  )
                }
                _ <- fileSystem.writeBytes(files.headerPath, header)
              yield NiftiFiles.PairFile(
                files.headerPath,
                files.payloadPath
              )
      yield written

  private def writeHeader[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      A,
      Sem,
      R <: AnyRank
  ](
      buffer: ByteBuffer,
      dimensions: Vector[Int],
      image: Sampled[S, A, Sem, R],
      storage: NiftiStorage,
      voxelOffset: Int,
      options: NiftiWriteOptions
  ): Unit =
    buffer.putInt(0, HeaderSize)
    buffer.putShort(40, dimensions.length.toShort)
    var axis = 0
    while axis < 7 do
      buffer.putShort(
        42 + axis * 2,
        (if axis < dimensions.length then dimensions(axis) else 1).toShort
      )
      axis += 1
    buffer.putShort(70, options.datatype.code.toShort)
    buffer.putShort(72, options.datatype.bitsPerValue.toShort)
    buffer.putFloat(76, 1.0f)
    val affine = image.grid.indexToFrame.matrix
    axis = 0
    while axis < 3 do
      var squared = 0.0
      var row = 0
      while row < 3 do
        squared += affine(row, axis) * affine(row, axis)
        row += 1
      buffer.putFloat(80 + axis * 4, math.sqrt(squared).toFloat)
      axis += 1
    while axis < dimensions.length do
      buffer.putFloat(
        80 + axis * 4,
        options.nonSpatialPixelDimensions
          .lift(axis - 3)
          .getOrElse(1.0)
          .toFloat
      )
      axis += 1
    buffer.putFloat(108, voxelOffset.toFloat)
    buffer.putFloat(112, options.slope.toFloat)
    buffer.putFloat(116, options.intercept.toFloat)
    buffer.put(
      123,
      (
        spatialUnitCode(image.frame.unit) |
          temporalUnitCode(options.temporalUnit)
      ).toByte
    )
    buffer.putShort(254, 1.toShort)
    var row = 0
    while row < 3 do
      var column = 0
      while column < 4 do
        buffer.putFloat(
          280 + row * 16 + column * 4,
          affine(row, column).toFloat
        )
        column += 1
      row += 1
    buffer.put(344, 'n'.toByte)
    buffer.put(
      345,
      (storage match
        case NiftiStorage.SingleFile => '+'
        case NiftiStorage.PairFile   => 'i').toByte
    )
    buffer.put(346, '1'.toByte)
    buffer.put(347, 0.toByte)
    ()

  private def writeExtensions(
      buffer: ByteBuffer,
      extensions: Vector[NiftiExtension]
  ): Unit =
    buffer.put(
      HeaderSize,
      (if extensions.isEmpty then 0 else 1).toByte
    )
    buffer.put(HeaderSize + 1, 0.toByte)
    buffer.put(HeaderSize + 2, 0.toByte)
    buffer.put(HeaderSize + 3, 0.toByte)
    var offset = DataOffset
    extensions.foreach { extension =>
      buffer.putInt(offset, extension.encodedSize)
      buffer.putInt(offset + 4, extension.code)
      var index = 0
      while index < extension.payload.length do
        buffer.put(
          offset + ExtensionHeaderSize + index,
          extension.payload(index)
        )
        index += 1
      offset += extension.encodedSize
    }

  private def writeFileOrderedChunk[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      A,
      Sem,
      R <: AnyRank
  ](
      bytes: Array[Byte],
      length: Int,
      payloadOffset: Long,
      dimensions: Vector[Int],
      image: Sampled[S, A, Sem, R],
      options: NiftiWriteOptions,
      source: WriteValueSource[A]
  ): Either[NiftiError, Unit] =
    val buffer =
      ByteBuffer
        .wrap(bytes, 0, length)
        .order(ByteOrder.LITTLE_ENDIAN)
    val indices = new Array[Int](dimensions.length)
    val bytesPerValue = options.datatype.bitsPerValue / 8
    var fileIndex = (payloadOffset / bytesPerValue.toLong).toInt
    var byteOffset = 0
    var failure: Option[NiftiError] = None
    while byteOffset < length && failure.isEmpty do
      decodeFirstAxisFastest(fileIndex, dimensions, indices)
      val value = source.read(image.data, indices)
      try
        writeEncodedValue(
          buffer,
          byteOffset,
          indices,
          value,
          options
        )
        fileIndex += 1
        byteOffset += bytesPerValue
      catch
        case conversion: WriteConversionFailure =>
          failure = Some(conversion.error)
    failure.toLeft(())

  private def validateFileOrderedValues[
      F <: Frame[D3],
      S <: SampleSpace[F, D3],
      A,
      Sem,
      R <: AnyRank
  ](
      dimensions: Vector[Int],
      image: Sampled[S, A, Sem, R],
      options: NiftiWriteOptions,
      source: WriteValueSource[A]
  ): Either[NiftiError, Unit] =
    val bytes = new Array[Byte](8)
    val buffer =
      ByteBuffer
        .wrap(bytes)
        .order(ByteOrder.LITTLE_ENDIAN)
    val indices = new Array[Int](dimensions.length)
    var fileIndex = 0
    var failure: Option[NiftiError] = None
    while fileIndex < image.data.size && failure.isEmpty do
      decodeFirstAxisFastest(fileIndex, dimensions, indices)
      val value = source.read(image.data, indices)
      try
        writeEncodedValue(
          buffer,
          0,
          indices,
          value,
          options
        )
        fileIndex += 1
      catch
        case conversion: WriteConversionFailure =>
          failure = Some(conversion.error)
    failure.toLeft(())

  /** Concrete write sources preserve primitive Ravel access in both the
    * validation and emission passes. Keeping the element type abstract here
    * would route rank-specific reads through the generic boxed fallback on
    * some JVMs.
    */
  private trait WriteValueSource[A]:
    def read[R <: AnyRank](
        data: NDArray[A, R],
        indices: Array[Int]
    ): Double

  private object WriteValueSource:
    val double: WriteValueSource[Double] =
      new WriteValueSource[Double]:
        def read[R <: AnyRank](
            data: NDArray[Double, R],
            indices: Array[Int]
        ): Double =
          indices.length match
            case 3 =>
              data(indices(0), indices(1), indices(2))
            case 4 =>
              data(indices(0), indices(1), indices(2), indices(3))
            case _ =>
              data.at(IArray.unsafeFromArray(indices))

    val long: WriteValueSource[Long] =
      new WriteValueSource[Long]:
        def read[R <: AnyRank](
            data: NDArray[Long, R],
            indices: Array[Int]
        ): Double =
          indices.length match
            case 3 =>
              data(indices(0), indices(1), indices(2)).toDouble
            case 4 =>
              data(indices(0), indices(1), indices(2), indices(3)).toDouble
            case _ =>
              data.at(IArray.unsafeFromArray(indices)).toDouble

  private def writeEncodedValue(
      buffer: ByteBuffer,
      offset: Int,
      logicalIndex: Array[Int],
      value: Double,
      options: NiftiWriteOptions
  ): Unit =
    val encoded =
      (value - options.intercept) / options.slope
    options.datatype match
      case NiftiDatatype.UInt8 =>
        val integer = integerValue(
          logicalIndex,
          value,
          encoded,
          options,
          minimum = 0.0,
          maximum = 255.0
        )
        val _ = buffer.put(offset, integer.toInt.toByte)
      case NiftiDatatype.Int16 =>
        val integer = integerValue(
          logicalIndex,
          value,
          encoded,
          options,
          minimum = Short.MinValue.toDouble,
          maximum = Short.MaxValue.toDouble
        )
        val _ = buffer.putShort(offset, integer.toInt.toShort)
      case NiftiDatatype.Int32 =>
        val integer = integerValue(
          logicalIndex,
          value,
          encoded,
          options,
          minimum = Int.MinValue.toDouble,
          maximum = Int.MaxValue.toDouble
        )
        val _ = buffer.putInt(offset, integer.toInt)
      case NiftiDatatype.Float32 =>
        val narrowed = encoded.toFloat
        if encoded.isFinite && !narrowed.isFinite then
          throw WriteConversionFailure(
            valueError(
              logicalIndex,
              value,
              encoded,
              options.datatype,
              NiftiValueProblem.FloatingOverflow
            )
          )
        else
          val _ = buffer.putFloat(offset, narrowed)
      case NiftiDatatype.Float64 =>
        if value.isFinite && !encoded.isFinite then
          throw WriteConversionFailure(
            valueError(
              logicalIndex,
              value,
              encoded,
              options.datatype,
              NiftiValueProblem.FloatingOverflow
            )
          )
        else
          val _ = buffer.putDouble(offset, encoded)

  private def integerValue(
      logicalIndex: Array[Int],
      value: Double,
      encoded: Double,
      options: NiftiWriteOptions,
      minimum: Double,
      maximum: Double
  ): Double =
    if !encoded.isFinite then
      throw WriteConversionFailure(
        valueError(
          logicalIndex,
          value,
          encoded,
          options.datatype,
          NiftiValueProblem.NonFinite
        )
      )
    else
      val rounded = math.rint(encoded)
      if options.integerConversion ==
          NiftiIntegerConversion.RejectLossy &&
        encoded != rounded
      then
        throw WriteConversionFailure(
          valueError(
            logicalIndex,
            value,
            encoded,
            options.datatype,
            NiftiValueProblem.Fractional
          )
        )
      else if rounded < minimum || rounded > maximum then
        throw WriteConversionFailure(
          valueError(
            logicalIndex,
            value,
            encoded,
            options.datatype,
            NiftiValueProblem.OutsideRange(minimum, maximum)
          )
        )
      else rounded

  private def valueError(
      logicalIndex: Array[Int],
      value: Double,
      encoded: Double,
      datatype: NiftiDatatype,
      problem: NiftiValueProblem
  ): NiftiError =
    NiftiError.ValueNotRepresentable(
      logicalIndex.toVector,
      value,
      encoded,
      datatype,
      problem
    )

  private def validateIoLimits(
      limits: NiftiIoLimits
  ): Either[NiftiError, Unit] =
    Vector(
      "workingBufferBytes" -> limits.workingBufferBytes.toLong,
      "maximumPayloadBytes" -> limits.maximumPayloadBytes,
      "maximumDecodedBytes" -> limits.maximumDecodedBytes,
      "maximumExtensionBytes" -> limits.maximumExtensionBytes.toLong
    ).collectFirst {
      case (name, value) if value <= 0L =>
        NiftiError.InvalidIoLimit(name, value)
    }.toLeft(())

  private def validatePayloadResources[A](
      shape: Shape[AnyRank],
      datatype: NiftiDatatype,
      dtype: DType[A],
      limits: NiftiIoLimits
  ): Either[NiftiError, Unit] =
    val payloadBytes =
      shape.size.toLong * (datatype.bitsPerValue / 8).toLong
    val decodedBytes =
      shape.size.toLong * dtypeBytes(dtype).toLong
    if payloadBytes > limits.maximumPayloadBytes then
      Left(
        NiftiError.PayloadResourceLimitExceeded(
          NiftiOperation.ReadPayload,
          payloadBytes,
          limits.maximumPayloadBytes
        )
      )
    else if decodedBytes > limits.maximumDecodedBytes then
      Left(
        NiftiError.DecodedResourceLimitExceeded(
          decodedBytes,
          limits.maximumDecodedBytes,
          dtype.name
        )
      )
    else Right(())

  private def dtypeBytes[A](dtype: DType[A]): Int =
    dtype.name match
      case "Boolean" | "Byte" => 1
      case "Short"            => 2
      case "Int" | "Float"    => 4
      case "Long" | "Double"  => 8
      case _                  => 8

  private def alignedWorkingBuffer(
      configured: Int,
      bytesPerValue: Int
  ): Int =
    math.max(
      bytesPerValue,
      (configured / bytesPerValue) * bytesPerValue
    )

  private def readExactBytes(
      path: P,
      operation: NiftiOperation,
      startOffset: Long,
      byteCount: Int,
      workingBufferBytes: Int
  ): Either[NiftiError, Array[Byte]] =
    val output = new Array[Byte](byteCount)
    var destination = 0
    fileSystem
      .readChunks(
        path,
        operation,
        startOffset,
        byteCount.toLong,
        workingBufferBytes
      ) { (bytes, length) =>
        System.arraycopy(bytes, 0, output, destination, length)
        destination += length
        Right(())
      }
      .map(_ => output)

  private def decodeFirstAxisFastest(
      offset: Int,
      dimensions: Vector[Int],
      output: Array[Int]
  ): Unit =
    var remaining = offset
    var axis = 0
    while axis < dimensions.length do
      output(axis) = remaining % dimensions(axis)
      remaining /= dimensions(axis)
      axis += 1

  private def lastAxisFastestOffset(
      indices: Array[Int],
      dimensions: Vector[Int]
  ): Int =
    var offset = 0
    var axis = 0
    while axis < dimensions.length do
      offset = offset * dimensions(axis) + indices(axis)
      axis += 1
    offset

  // Private exception trampolines escape callback/Unit-returning codec loops.
  // Every throw site is caught inside this object and converted back to the
  // public NiftiError Either channel before control reaches a caller.
  private final case class ReadConversionFailure(
      error: NiftiError
  ) extends RuntimeException(error.message)

  private final case class WriteConversionFailure(
      error: NiftiError
  ) extends RuntimeException(error.message)

  private def readRawValue(
      buffer: ByteBuffer,
      offset: Int,
      datatype: NiftiDatatype
  ): Double =
    datatype match
      case NiftiDatatype.UInt8 =>
        (buffer.get(offset) & 0xff).toDouble
      case NiftiDatatype.Int16 =>
        buffer.getShort(offset).toDouble
      case NiftiDatatype.Int32 =>
        buffer.getInt(offset).toDouble
      case NiftiDatatype.Float32 =>
        buffer.getFloat(offset).toDouble
      case NiftiDatatype.Float64 =>
        buffer.getDouble(offset)

  private def unsignedShort(
      buffer: ByteBuffer,
      offset: Int
  ): Int =
    buffer.getShort(offset).toInt & 0xffff

  private def javaOrder(order: NiftiByteOrder): ByteOrder =
    order match
      case NiftiByteOrder.LittleEndian => ByteOrder.LITTLE_ENDIAN
      case NiftiByteOrder.BigEndian    => ByteOrder.BIG_ENDIAN

  private def spatialUnit(code: Int): NiftiSpatialUnit =
    code match
      case 1 => NiftiSpatialUnit.Meter
      case 2 => NiftiSpatialUnit.Millimeter
      case 3 => NiftiSpatialUnit.Micrometer
      case _ => NiftiSpatialUnit.Unknown

  private def temporalUnit(code: Int): NiftiTemporalUnit =
    code match
      case 8  => NiftiTemporalUnit.Second
      case 16 => NiftiTemporalUnit.Millisecond
      case 24 => NiftiTemporalUnit.Microsecond
      case 32 => NiftiTemporalUnit.Hertz
      case 40 => NiftiTemporalUnit.Ppm
      case 48 => NiftiTemporalUnit.RadianPerSecond
      case _  => NiftiTemporalUnit.Unknown

  private def geometryUnit(
      unit: NiftiSpatialUnit,
      fallback: LengthUnit
  ): LengthUnit =
    unit match
      case NiftiSpatialUnit.Meter      => LengthUnit.Meter
      case NiftiSpatialUnit.Micrometer => LengthUnit.Micrometer
      case NiftiSpatialUnit.Millimeter => LengthUnit.Millimeter
      case NiftiSpatialUnit.Unknown    => fallback

  private def spatialUnitCode(unit: LengthUnit): Int =
    unit match
      case LengthUnit.Meter      => 1
      case LengthUnit.Millimeter => 2
      case LengthUnit.Micrometer => 3

  private def temporalUnitCode(unit: NiftiTemporalUnit): Int =
    unit match
      case NiftiTemporalUnit.Unknown         => 0
      case NiftiTemporalUnit.Second          => 8
      case NiftiTemporalUnit.Millisecond     => 16
      case NiftiTemporalUnit.Microsecond     => 24
      case NiftiTemporalUnit.Hertz           => 32
      case NiftiTemporalUnit.Ppm             => 40
      case NiftiTemporalUnit.RadianPerSecond => 48

  private final case class ResolvedFiles(
      entryPath: P,
      storage: NiftiStorage,
      headerPath: P,
      payloadPath: P
  )

  private def resolveFiles(
      path: P
  ): Either[NiftiError, ResolvedFiles] =
    val name = fileSystem.fileName(path)
    val lower = name.toLowerCase
    if lower.endsWith(".nii.gz") || lower.endsWith(".nii") then
      Right(
        ResolvedFiles(
          path,
          NiftiStorage.SingleFile,
          path,
          path
        )
      )
    else if lower.endsWith(".hdr.gz") then
      Right(
        ResolvedFiles(
          path,
          NiftiStorage.PairFile,
          path,
          replaceSuffix(path, ".hdr.gz", ".img.gz")
        )
      )
    else if lower.endsWith(".img.gz") then
      Right(
        ResolvedFiles(
          path,
          NiftiStorage.PairFile,
          replaceSuffix(path, ".img.gz", ".hdr.gz"),
          path
        )
      )
    else if lower.endsWith(".hdr") then
      Right(
        ResolvedFiles(
          path,
          NiftiStorage.PairFile,
          path,
          replaceSuffix(path, ".hdr", ".img")
        )
      )
    else if lower.endsWith(".img") then
      Right(
        ResolvedFiles(
          path,
          NiftiStorage.PairFile,
          replaceSuffix(path, ".img", ".hdr"),
          path
        )
      )
    else
      Left(NiftiError.UnsupportedPath(fileSystem.show(path)))

  private def resolveForRead(
      path: P
  ): Either[NiftiError, ResolvedFiles] =
    resolveFiles(path).flatMap { files =>
      if files.storage == NiftiStorage.PairFile &&
        fileSystem.exists(files.entryPath)
      then
        val companion =
          if files.entryPath == files.headerPath then
            files.payloadPath
          else files.headerPath
        if fileSystem.exists(companion) then Right(files)
        else
          Left(
            NiftiError.MissingCompanion(
              fileSystem.show(files.entryPath),
              fileSystem.show(companion)
            )
          )
      else Right(files)
    }

  private def replaceSuffix(
      path: P,
      suffix: String,
      replacement: String
  ): P =
    val name = fileSystem.fileName(path)
    fileSystem.sibling(
      path,
      name.substring(0, name.length - suffix.length) + replacement
    )

  private val HeaderSize = 348
  private val DataOffset = 352
  private val ExtensionFlagSize = 4
  private val ExtensionHeaderSize = 8
  private val MinimumExtensionSize = 16
  private val ExtensionAlignment = 16
  private val LongUpperExclusive = 9223372036854775808.0
