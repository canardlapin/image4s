package image4s.nifti

/** Writes single-file NIfTI-1 transform containers (little-endian, float64 data), so fields round-trip value-exactly.
  *
  * @param dims
  *   the extents of dimensions 1..n (n <= 7)
  * @param values
  *   data in NIfTI order (x fastest)
  */
object Nifti1Writer:
  final case class Header(
      dims: Vector[Int],
      pixdim: Vector[Double],
      intentCode: Int = 0,
      intentP: Vector[Double] = Vector(0.0, 0.0, 0.0),
      qformCode: Int = 0,
      quatern: Vector[Double] = Vector(0.0, 0.0, 0.0),
      qoffset: Vector[Double] = Vector(0.0, 0.0, 0.0),
      qfac: Double = 1.0,
      sformCode: Int = 0,
      srow: Vector[Double] = Vector.fill(12)(0.0),
      intentName: String = ""
  ):
    require(dims.nonEmpty && dims.size <= 7 && dims.forall(_ > 0), s"invalid NIfTI dims $dims")
    require(dims.forall(_ <= Short.MaxValue), "NIfTI-1 extents must fit a signed 16-bit field")
    require(pixdim.size >= 3 && pixdim.size <= 7, "pixdim needs three to seven spacings")
    require(intentP.size == 3 && quatern.size == 3 && qoffset.size == 3 && srow.size == 12, "invalid raw NIfTI metadata dimensions")

  /** The header of an existing container, for re-writing its data. */
  def headerOf(raw: Nifti1Container): Header =
    Header(
      dims = raw.shape,
      pixdim = raw.pixdim.slice(1, 1 + raw.shape.size).padTo(3, 1.0),
      intentCode = raw.intentCode,
      intentP = raw.intentP,
      qformCode = raw.qformCode,
      quatern = raw.quatern,
      qoffset = raw.qoffset,
      qfac = if raw.pixdim(0) < 0.0 then -1.0 else 1.0,
      sformCode = raw.sformCode,
      srow = raw.srow,
      intentName = raw.intentName
    )

  def write(header: Header, values: Array[Double]): IArray[Byte] =
    val count = header.dims.foldLeft(BigInt(1))(_ * _)
    require(BigInt(values.length) == count, s"expected $count values, got ${values.length}")
    require(values.length <= (Int.MaxValue - 352) / 8, "NIfTI output exceeds one byte array")
    val voxOffset = 352
    val out = new Array[Byte](voxOffset + 8 * values.length)
    def i16(at: Int, v: Int): Unit =
      out(at) = v.toByte
      out(at + 1) = (v >>> 8).toByte
    def i32(at: Int, v: Int): Unit =
      var i = 0
      while i < 4 do
        out(at + i) = (v >>> (8 * i)).toByte
        i += 1
    def f32(at: Int, v: Double): Unit = i32(at, java.lang.Float.floatToIntBits(v.toFloat))
    def text(at: Int, s: String, max: Int): Unit =
      s.take(max - 1).zipWithIndex.foreach((c, i) => out(at + i) = c.toByte)
    i32(0, 348)
    i16(40, header.dims.size)
    header.dims.zipWithIndex.foreach((d, i) => i16(42 + 2 * i, d))
    (header.dims.size until 7).foreach(i => i16(42 + 2 * i, 1))
    header.intentP.zipWithIndex.foreach((p, i) => f32(56 + 4 * i, p))
    i16(68, header.intentCode)
    i16(70, Nifti1Container.Float64)
    i16(72, 64)
    f32(76, header.qfac)
    header.pixdim.zipWithIndex.foreach((p, i) => f32(80 + 4 * i, p))
    ((header.pixdim.size + 1) until 8).foreach(i => f32(76 + 4 * i, 1.0))
    f32(108, voxOffset.toDouble)
    f32(112, 1.0) // scl_slope
    f32(116, 0.0)
    out(123) = 10 // xyzt_units: mm + seconds
    i16(252, header.qformCode)
    i16(254, header.sformCode)
    header.quatern.zipWithIndex.foreach((q, i) => f32(256 + 4 * i, q))
    header.qoffset.zipWithIndex.foreach((q, i) => f32(268 + 4 * i, q))
    header.srow.zipWithIndex.foreach((s, i) => f32(280 + 4 * i, s))
    text(328, header.intentName, 16)
    text(344, "n+1", 4)
    var i = 0
    while i < values.length do
      val bits = java.lang.Double.doubleToRawLongBits(values(i))
      var b = 0
      while b < 8 do
        out(voxOffset + 8 * i + b) = (bits >>> (8 * b)).toByte
        b += 1
      i += 1
    IArray.unsafeFromArray(out)
