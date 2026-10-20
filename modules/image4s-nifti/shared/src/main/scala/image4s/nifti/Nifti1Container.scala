package image4s.nifti


/** A single-file NIfTI-1 image decoded from uncompressed bytes, keeping the raw header fields transform formats rely on
  * (intent codes and parameters, quaternion and srow fields, pixdim) that general image readers normalise away.
  *
  * This raw format boundary preserves uninterpreted metadata and first-axis-fastest payload order on JVM and JS.
  * It does not choose an anatomical convention or construct an image; use [[Nifti]] for normalized image admission.
  */
final class Nifti1Container private (
    val littleEndian: Boolean,
    val dim: Vector[Int],
    val intentP: Vector[Double],
    val intentCode: Int,
    val datatype: Int,
    val bitpix: Int,
    val pixdim: Vector[Double],
    val voxOffset: Int,
    val sclSlope: Double,
    val sclInter: Double,
    val qformCode: Int,
    val sformCode: Int,
    val quatern: Vector[Double],
    val qoffset: Vector[Double],
    val srow: Vector[Double],
    val intentName: String,
    private val bytes: IArray[Byte]
):
  /** Extents of dimensions 1..dim[0]. */
  def shape: Vector[Int] =
    dim.slice(1, 1 + dim(0))

  def spatialShape: Vector[Int] =
    shape.padTo(3, 1).take(3)

  val voxelCount: Long =
    shape.foldLeft(1L)(_ * _)

  /** The quaternion (qform) voxel-to-world affine, row-major 4x4, per the NIfTI-1 standard. */
  def qformRowMajor: Vector[Double] =
    val (b, c, d) = (quatern(0), quatern(1), quatern(2))
    val residual = 1.0 - (b * b + c * c + d * d)
    val a = if residual < 1e-7 then 0.0 else math.sqrt(residual)
    val (bn, cn, dn) =
      if residual < 1e-7 then
        val norm = math.sqrt(b * b + c * c + d * d)
        (b / norm, c / norm, d / norm)
      else (b, c, d)
    val qfac = if pixdim(0) < 0.0 then -1.0 else 1.0
    val (dx, dy, dz) = (pixdim(1), pixdim(2), pixdim(3) * qfac)
    val r = Vector(
      a * a + bn * bn - cn * cn - dn * dn, 2 * (bn * cn - a * dn), 2 * (bn * dn + a * cn),
      2 * (bn * cn + a * dn), a * a + cn * cn - bn * bn - dn * dn, 2 * (cn * dn - a * bn),
      2 * (bn * dn - a * cn), 2 * (cn * dn + a * bn), a * a + dn * dn - cn * cn - bn * bn
    )
    Vector(
      r(0) * dx, r(1) * dy, r(2) * dz, qoffset(0),
      r(3) * dx, r(4) * dy, r(5) * dz, qoffset(1),
      r(6) * dx, r(7) * dy, r(8) * dz, qoffset(2),
      0.0, 0.0, 0.0, 1.0
    )

  /** The srow (sform) voxel-to-world affine, row-major 4x4. */
  def sformRowMajor: Vector[Double] =
    srow ++ Vector(0.0, 0.0, 0.0, 1.0)

  /** Scaled value at a flat voxel index (x fastest). */
  def value(flat: Long): Double =
    val raw = rawValue(flat)
    if sclSlope != 0.0 && !(sclSlope == 1.0 && sclInter == 0.0) then raw * sclSlope + sclInter else raw

  /** Scaled value at an index into all dimensions (missing trailing indices are 0). */
  def value(index: Int*): Double =
    val extents = shape
    var flat = 0L
    var stride = 1L
    var axis = 0
    while axis < extents.size do
      val i = if axis < index.size then index(axis) else 0
      require(i >= 0 && i < extents(axis), s"index $i out of bounds for axis $axis (extent ${extents(axis)})")
      flat += i * stride
      stride *= extents(axis)
      axis += 1
    value(flat)

  private def rawValue(flat: Long): Double =
    if flat < 0L || flat >= voxelCount then throw new IllegalArgumentException(s"voxel index $flat outside [0, $voxelCount)")
    val size = bitpix / 8
    val at = voxOffset + (flat * size).toInt
    datatype match
      case Nifti1Container.Float32 => java.lang.Float.intBitsToFloat(Nifti1Container.int32(bytes, at, littleEndian)).toDouble
      case Nifti1Container.Float64 => java.lang.Double.longBitsToDouble(Nifti1Container.int64(bytes, at, littleEndian))
      case Nifti1Container.Int16   => Nifti1Container.int16(bytes, at, littleEndian).toDouble
      case Nifti1Container.Int32   => Nifti1Container.int32(bytes, at, littleEndian).toDouble
      case Nifti1Container.UInt8   => (bytes(at) & 0xff).toDouble
      case other            => throw new IllegalStateException(s"datatype $other admitted without a reader")

object Nifti1Container:
  val Float32 = 16
  val Float64 = 64
  val Int16 = 4
  val Int32 = 8
  val UInt8 = 2

  private val HeaderSize = 348

  def parse(bytes: IArray[Byte]): Either[NiftiContainerError, Nifti1Container] =
    if bytes.length < HeaderSize then Left(NiftiContainerError(s"only ${bytes.length} bytes"))
    else
      val little =
        if int32(bytes, 0, littleEndian = true) == HeaderSize then Some(true)
        else if int32(bytes, 0, littleEndian = false) == HeaderSize then Some(false)
        else None
      little match
        case None => Left(NiftiContainerError("sizeof_hdr is not 348 (not NIfTI-1)"))
        case Some(le) =>
          val magic = Nifti1Container.latin1(bytes, 344, 347)
          if magic != "n+1" then Left(NiftiContainerError(s"magic '$magic' is not single-file NIfTI-1 'n+1'"))
          else
            def i16(at: Int) = int16(bytes, at, le).toInt
            def f32(at: Int) = java.lang.Float.intBitsToFloat(int32(bytes, at, le)).toDouble
            val dim = Vector.tabulate(8)(i => i16(40 + 2 * i))
            val datatype = i16(70)
            val bitpix = i16(72)
            val offset = f32(108)
            val voxOffset = offset.toInt
            val supported = Set(Float32, Float64, Int16, Int32, UInt8)
            if dim(0) < 1 || dim(0) > 7 || dim.slice(1, 1 + dim(0)).exists(_ <= 0) then
              Left(NiftiContainerError(s"invalid dim $dim"))
            else if !supported.contains(datatype) then Left(NiftiContainerError(s"datatype $datatype"))
            else if bitpix != (datatype match
                case Float32 | Int32 => 32
                case Float64 => 64
                case Int16 => 16
                case _ => 8) then Left(NiftiContainerError(s"bitpix $bitpix does not match datatype $datatype"))
            else if !offset.isFinite || offset != voxOffset.toDouble || voxOffset < 352 then
              Left(NiftiContainerError(s"invalid single-file vox_offset $offset"))
            else if BigInt(voxOffset) + dim.slice(1, 1 + dim(0)).foldLeft(BigInt(1))(_ * _) * (bitpix / 8) > bytes.length then
              Left(NiftiContainerError(s"data exceeds file size ${bytes.length}"))
            else
              val raw = new Nifti1Container(
                littleEndian = le,
                dim = dim,
                intentP = Vector(f32(56), f32(60), f32(64)),
                intentCode = i16(68),
                datatype = datatype,
                bitpix = bitpix,
                pixdim = Vector.tabulate(8)(i => f32(76 + 4 * i)),
                voxOffset = voxOffset,
                sclSlope = f32(112),
                sclInter = f32(116),
                qformCode = i16(252),
                sformCode = i16(254),
                quatern = Vector(f32(256), f32(260), f32(264)),
                qoffset = Vector(f32(268), f32(272), f32(276)),
                srow = Vector.tabulate(12)(i => f32(280 + 4 * i)),
                intentName = Nifti1Container.latin1(bytes, 328, 344).takeWhile(_ != '\u0000'),
                bytes = bytes
              )
              Right(raw)

  private[nifti] def latin1(b: IArray[Byte], from: Int, until: Int): String =
    val out = new StringBuilder
    var i = from
    while i < until do
      out.append((b(i) & 0xff).toChar)
      i += 1
    out.toString

  private[nifti] def int16(b: IArray[Byte], at: Int, littleEndian: Boolean): Short =
    if littleEndian then ((b(at) & 0xff) | ((b(at + 1) & 0xff) << 8)).toShort
    else (((b(at) & 0xff) << 8) | (b(at + 1) & 0xff)).toShort

  private[nifti] def int32(b: IArray[Byte], at: Int, littleEndian: Boolean): Int =
    if littleEndian then (b(at) & 0xff) | ((b(at + 1) & 0xff) << 8) | ((b(at + 2) & 0xff) << 16) | ((b(at + 3) & 0xff) << 24)
    else ((b(at) & 0xff) << 24) | ((b(at + 1) & 0xff) << 16) | ((b(at + 2) & 0xff) << 8) | (b(at + 3) & 0xff)

  private[nifti] def int64(b: IArray[Byte], at: Int, littleEndian: Boolean): Long =
    val lo = int32(b, if littleEndian then at else at + 4, littleEndian).toLong & 0xffffffffL
    val hi = int32(b, if littleEndian then at + 4 else at, littleEndian).toLong
    (hi << 32) | lo

/** A raw single-file NIfTI-1 container failed admission before payload access. */
final case class NiftiContainerError(message: String) derives CanEqual
