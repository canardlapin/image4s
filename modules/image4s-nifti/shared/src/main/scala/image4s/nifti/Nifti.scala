package image4s.nifti

import image4s.Axis
import image4s.AxisKind
import image4s.FieldRole
import image4s.Label
import image4s.LabelImage
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.Scalar
import image4s.ScalarImage
import image4s.SomeSampled
import ravel.AnyRank
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameMetadata
import image4s.geometry.Grid
import image4s.geometry.LengthUnit

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets

private[nifti] final class NiftiApi[P](
    fileSystem: NiftiFileSystem[P]
):
  def readHeader(path: P): Either[NiftiError, NiftiHeader] =
    for
      files <- resolveForRead(path)
      header <- readResolvedHeader(files)
    yield header

  def readScalar(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[Double, Scalar]]
  ] =
    for
      header <- readHeader(path)
      frame <- freshFrame(path, header, options)
      decoded <- readIn[Scalar](
        path,
        frame,
        header
      )
    yield DecodedNifti(SomeSampled.d3(decoded), header)

  def readLabels(
      path: P,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiError,
    DecodedNifti[SomeSampled[Double, Label]]
  ] =
    for
      header <- readHeader(path)
      frame <- freshFrame(path, header, options)
      decoded <- readIn[Label](
        path,
        frame,
        header
      )
    yield DecodedNifti(SomeSampled.d3(decoded), header)

  def readScalarIn[F <: Frame[D3]](
      path: P,
      frame: F
  ): Either[
    NiftiError,
    DecodedNifti[ScalarImage[frame.type, D3, AnyRank]]
  ] =
    for
      header <- readHeader(path)
      _ <- validateSuppliedFrame(frame, header)
      decoded <- readIn[Scalar](path, frame, header)
    yield DecodedNifti(decoded, header)

  def readLabelsIn[F <: Frame[D3]](
      path: P,
      frame: F
  ): Either[
    NiftiError,
    DecodedNifti[LabelImage[frame.type, D3, Double, AnyRank]]
  ] =
    for
      header <- readHeader(path)
      _ <- validateSuppliedFrame(frame, header)
      decoded <- readIn[Label](path, frame, header)
    yield DecodedNifti(decoded, header)

  def writeScalar[F <: Frame[D3], R <: AnyRank](
      path: P,
      image: ScalarImage[F, D3, R],
      options: NiftiWriteOptions = NiftiWriteOptions.default,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[P]] =
    writeDouble(path, image, options, extensions)

  def writeLabels[F <: Frame[D3], R <: AnyRank](
      path: P,
      image: LabelImage[F, D3, Double, R],
      options: NiftiWriteOptions = NiftiWriteOptions.default,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[P]] =
    writeDouble(path, image, options, extensions)

  private def readIn[
      Role <: FieldRole
  ](
      path: P,
      frame: Frame[D3],
      header: NiftiHeader
  ): Either[
    NiftiError,
    Sampled[frame.type, D3, Double, Role, AnyRank]
  ] =
    for
      axes <- axesFrom(header)
      shape <- Shape
        .from(header.logicalShape)
        .left
        .map(error => NiftiError.InvalidArrayShape(error.toString))
      fileOrdered <- readPayload(path, header)
      logicalData = NDArray.fromSeq(
        shape,
        cOrderValues(header.logicalShape, fileOrdered)
      )
      grid <- Grid
        .in(frame)(header.spatialShape, header.preferredAffine)
        .left
        .map(NiftiError.Geometry.apply)
      sampled <- Sampled
        .create[frame.type, D3, Double, Role, AnyRank](
          grid,
          axes,
          logicalData
        )
        .left
        .map(NiftiError.Image.apply)
    yield sampled

  private def axesFrom(
      header: NiftiHeader
  ): Either[NiftiError, NonSpatialAxes] =
    val created =
      header.nonSpatialShape.zipWithIndex.foldLeft[
        Either[NiftiError, Vector[Axis]]
      ](Right(Vector.empty)) { case (accumulated, (extent, index)) =>
        for
          axes <- accumulated
          axis <- Axis
            .create(
              s"nifti-axis-${index + 4}",
              extent,
              AxisKind.Other
            )
            .left
            .map(NiftiError.Image.apply)
        yield axes :+ axis
      }
    created.flatMap { axes =>
      NonSpatialAxes
        .from(axes)
        .left
        .map(NiftiError.Image.apply)
    }

  private def freshFrame(
      path: P,
      header: NiftiHeader,
      options: NiftiReadOptions
  ): Either[NiftiError, Frame[D3]] =
    val label =
      Option(fileSystem.fileName(path))
        .filter(_.nonEmpty)
        .getOrElse("nifti")
    FrameMetadata
      .create(
        label,
        geometryUnit(header.spatialUnit, options.fallbackSpatialUnit),
        CoordinateConvention.RAS
      )
      .left
      .map(NiftiError.Geometry.apply)
      .map(Frame.fresh[D3])

  private def validateSuppliedFrame(
      frame: Frame[D3],
      header: NiftiHeader
  ): Either[NiftiError, Unit] =
    if frame.metadata.convention != CoordinateConvention.RAS then
      Left(
        NiftiError.FrameConventionMismatch(frame.metadata.convention)
      )
    else
      header.spatialUnit match
        case NiftiSpatialUnit.Unknown =>
          Right(())
        case unit
            if geometryUnit(unit, frame.metadata.unit) !=
              frame.metadata.unit =>
          Left(
            NiftiError.FrameUnitMismatch(unit, frame.metadata.unit)
          )
        case _ =>
          Right(())

  private def readPayload(
      path: P,
      header: NiftiHeader
  ): Either[NiftiError, Array[Double]] =
    for
      files <- resolveForRead(path)
      bytes <- fileSystem.readBytes(
        files.payloadPath,
        NiftiOperation.ReadPayload
      )
      output = decodePayload(bytes, header)
      values <- output
    yield values

  private def decodePayload(
      bytes: Array[Byte],
      header: NiftiHeader
  ): Either[NiftiError, Array[Double]] =
    val values =
      new Array[Double](elementCount(header.dimensions))
    val bytesPerValue = header.datatype.bitsPerValue / 8
    val buffer =
      ByteBuffer.wrap(bytes).order(javaOrder(header.byteOrder))
    var index = 0
    var failure: Option[NiftiError] = None
    while index < values.length && failure.isEmpty do
      val offset = header.voxelOffset + index * bytesPerValue
      val available =
        math.max(0, math.min(bytesPerValue, bytes.length - offset))
      if available != bytesPerValue then
        failure =
          Some(
            NiftiError.UnexpectedEndOfFile(
              NiftiOperation.ReadPayload,
              bytesPerValue,
              available
            )
          )
      else
        val raw = readValue(buffer, offset, header.datatype)
        values(index) =
          raw * header.effectiveSlope + header.intercept
        index += 1
    failure.toLeft(values)

  private def readResolvedHeader(
      files: ResolvedFiles
  ): Either[NiftiError, NiftiHeader] =
    fileSystem
      .readBytes(files.headerPath, NiftiOperation.ReadHeader)
      .flatMap { allBytes =>
      if allBytes.length < HeaderSize then
        Left(
          NiftiError.UnexpectedEndOfFile(
            NiftiOperation.ReadHeader,
            HeaderSize,
            allBytes.length
          )
        )
      else
        val headerBytes = allBytes.take(HeaderSize)
        for
          header <- parseHeader(
            headerBytes,
            files.storage,
            files.headerPath
          )
          extensionRegion <- readExtensionRegion(allBytes, header)
          extensions <- parseExtensions(
            extensionRegion,
            header.byteOrder
          )
        yield header.copy(extensions = extensions)
    }

  private def readExtensionRegion(
      bytes: Array[Byte],
      header: NiftiHeader
  ): Either[NiftiError, Array[Byte]] =
    header.storage match
      case NiftiStorage.SingleFile =>
        val expected = header.voxelOffset - HeaderSize
        val available = math.max(0, bytes.length - HeaderSize)
        if available >= expected then
          Right(bytes.slice(HeaderSize, HeaderSize + expected))
        else
          Left(
            NiftiError.UnexpectedEndOfFile(
              NiftiOperation.ReadHeader,
              expected,
              available
            )
          )
      case NiftiStorage.PairFile =>
        Right(bytes.drop(HeaderSize))

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

  private def writeDouble[
      F <: Frame[D3],
      Role <: FieldRole,
      R <: AnyRank
  ](
      path: P,
      image: Sampled[F, D3, Double, Role, R],
      options: NiftiWriteOptions,
      extensions: Vector[NiftiExtension]
  ): Either[NiftiError, NiftiFiles[P]] =
    for
      files <- resolveFiles(path)
      result <- writeResolved(files, image, options, extensions)
    yield result

  private def writeResolved[
      F <: Frame[D3],
      Role <: FieldRole,
      R <: AnyRank
  ](
      files: ResolvedFiles,
      image: Sampled[F, D3, Double, Role, R],
      options: NiftiWriteOptions,
      extensions: Vector[NiftiExtension]
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
      val largestAllocation =
        files.storage match
          case NiftiStorage.SingleFile =>
            headerBytes + payloadBytes
          case NiftiStorage.PairFile =>
            math.max(headerBytes, payloadBytes)
      if largestAllocation > Int.MaxValue.toLong then
        Left(NiftiError.OutputTooLarge(largestAllocation))
      else
        files.storage match
          case NiftiStorage.SingleFile =>
            val bytes =
              new Array[Byte](
                (headerBytes + payloadBytes).toInt
              )
            val buffer =
              ByteBuffer
                .wrap(bytes)
                .order(ByteOrder.LITTLE_ENDIAN)
            writeHeader(
              buffer,
              dimensions,
              image,
              files.storage,
              headerBytes.toInt,
              options
            )
            writeExtensions(buffer, extensions)
            for
              _ <- writeFileOrderedValues(
                buffer,
                headerBytes.toInt,
                dimensions,
                image,
                options
              )
              _ <- fileSystem.writeBytes(files.headerPath, bytes)
            yield NiftiFiles.SingleFile(files.headerPath)
          case NiftiStorage.PairFile =>
            val header = new Array[Byte](headerBytes.toInt)
            val headerBuffer =
              ByteBuffer
                .wrap(header)
                .order(ByteOrder.LITTLE_ENDIAN)
            writeHeader(
              headerBuffer,
              dimensions,
              image,
              files.storage,
              0,
              options
            )
            writeExtensions(headerBuffer, extensions)
            val payload = new Array[Byte](payloadBytes.toInt)
            val payloadBuffer =
              ByteBuffer
                .wrap(payload)
                .order(ByteOrder.LITTLE_ENDIAN)
            for
              _ <- writeFileOrderedValues(
                payloadBuffer,
                0,
                dimensions,
                image,
                options
              )
              _ <- fileSystem.writeBytes(files.payloadPath, payload)
              _ <- fileSystem.writeBytes(files.headerPath, header)
            yield NiftiFiles.PairFile(
              files.headerPath,
              files.payloadPath
            )

  private def writeHeader[
      F <: Frame[D3],
      Role <: FieldRole,
      R <: AnyRank
  ](
      buffer: ByteBuffer,
      dimensions: Vector[Int],
      image: Sampled[F, D3, Double, Role, R],
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
        spatialUnitCode(image.frame.metadata.unit) |
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

  private def writeFileOrderedValues[
      F <: Frame[D3],
      Role <: FieldRole,
      R <: AnyRank
  ](
      buffer: ByteBuffer,
      payloadOffset: Int,
      dimensions: Vector[Int],
      image: Sampled[F, D3, Double, Role, R],
      options: NiftiWriteOptions
  ): Either[NiftiError, Unit] =
    val indices = new Array[Int](dimensions.length)
    var fileIndex = 0
    var failure: Option[NiftiError] = None
    val bytesPerValue = options.datatype.bitsPerValue / 8
    while fileIndex < image.data.size && failure.isEmpty do
      decodeFirstAxisFastest(fileIndex, dimensions, indices)
      val value =
        image.data.at(IArray.unsafeFromArray(indices))
      writeEncodedValue(
        buffer,
        payloadOffset + fileIndex * bytesPerValue,
        indices,
        value,
        options
      ) match
        case Left(error) =>
          failure = Some(error)
        case Right(_) =>
          fileIndex += 1
    failure.toLeft(())

  private def writeEncodedValue(
      buffer: ByteBuffer,
      offset: Int,
      logicalIndex: Array[Int],
      value: Double,
      options: NiftiWriteOptions
  ): Either[NiftiError, Unit] =
    val encoded =
      (value - options.intercept) / options.slope
    options.datatype match
      case NiftiDatatype.UInt8 =>
        integerValue(
          logicalIndex,
          value,
          encoded,
          options,
          minimum = 0.0,
          maximum = 255.0
        ).map { integer =>
          buffer.put(offset, integer.toInt.toByte)
          ()
        }
      case NiftiDatatype.Int16 =>
        integerValue(
          logicalIndex,
          value,
          encoded,
          options,
          minimum = Short.MinValue.toDouble,
          maximum = Short.MaxValue.toDouble
        ).map { integer =>
          buffer.putShort(offset, integer.toInt.toShort)
          ()
        }
      case NiftiDatatype.Int32 =>
        integerValue(
          logicalIndex,
          value,
          encoded,
          options,
          minimum = Int.MinValue.toDouble,
          maximum = Int.MaxValue.toDouble
        ).map { integer =>
          buffer.putInt(offset, integer.toInt)
          ()
        }
      case NiftiDatatype.Float32 =>
        val narrowed = encoded.toFloat
        if encoded.isFinite && !narrowed.isFinite then
          Left(
            valueError(
              logicalIndex,
              value,
              encoded,
              options.datatype,
              NiftiValueProblem.FloatingOverflow
            )
          )
        else
          buffer.putFloat(offset, narrowed)
          Right(())
      case NiftiDatatype.Float64 =>
        if value.isFinite && !encoded.isFinite then
          Left(
            valueError(
              logicalIndex,
              value,
              encoded,
              options.datatype,
              NiftiValueProblem.FloatingOverflow
            )
          )
        else
          buffer.putDouble(offset, encoded)
          Right(())

  private def integerValue(
      logicalIndex: Array[Int],
      value: Double,
      encoded: Double,
      options: NiftiWriteOptions,
      minimum: Double,
      maximum: Double
  ): Either[NiftiError, Double] =
    if !encoded.isFinite then
      Left(
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
        Left(
          valueError(
            logicalIndex,
            value,
            encoded,
            options.datatype,
            NiftiValueProblem.Fractional
          )
        )
      else if rounded < minimum || rounded > maximum then
        Left(
          valueError(
            logicalIndex,
            value,
            encoded,
            options.datatype,
            NiftiValueProblem.OutsideRange(minimum, maximum)
          )
        )
      else Right(rounded)

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

  private def cOrderValues(
      dimensions: Vector[Int],
      fileOrdered: Array[Double]
  ): IterableOnce[Double] =
    new Iterable[Double]:
      def iterator: Iterator[Double] =
        val indices = new Array[Int](dimensions.length)
        Iterator.range(0, fileOrdered.length).map { cIndex =>
          decodeLastAxisFastest(cIndex, dimensions, indices)
          fileOrdered(firstAxisFastestOffset(indices, dimensions))
        }

  private def decodeLastAxisFastest(
      offset: Int,
      dimensions: Vector[Int],
      output: Array[Int]
  ): Unit =
    var remaining = offset
    var axis = dimensions.length - 1
    while axis >= 0 do
      output(axis) = remaining % dimensions(axis)
      remaining /= dimensions(axis)
      axis -= 1

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

  private def firstAxisFastestOffset(
      indices: Array[Int],
      dimensions: Vector[Int]
  ): Int =
    var offset = 0
    var stride = 1
    var axis = 0
    while axis < dimensions.length do
      offset += indices(axis) * stride
      stride *= dimensions(axis)
      axis += 1
    offset

  private def readValue(
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

  private def elementCount(dimensions: Vector[Int]): Int =
    dimensions.foldLeft(1)(Math.multiplyExact)

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
