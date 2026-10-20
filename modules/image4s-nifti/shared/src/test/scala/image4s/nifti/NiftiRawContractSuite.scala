package image4s.nifti

class NiftiRawContractSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)

  private val header = Nifti1Writer.Header(
    dims = Vector(2, 3, 1, 1, 3), pixdim = Vector(2.0, 3.0, 4.0, 1.0, 1.0),
    intentCode = 1007, intentP = Vector(7.0, 8.0, 9.0),
    qformCode = 1, quatern = Vector(0.0, 0.0, 0.0), qoffset = Vector(5.0, -6.0, 7.0),
    sformCode = 2, srow = Vector(2.0, 0.0, 0.0, 11.0, 0.0, 3.0, 0.0, 12.0, 0.0, 0.0, 4.0, 13.0),
    intentName = "raw-vector"
  )
  private val values = Array.tabulate(18)(i => i.toDouble + 0.25)
  private def bytes: Array[Byte] = IArray.genericWrapArray(Nifti1Writer.write(header, values)).toArray
  private def i16(b: Array[Byte], at: Int, n: Int): Unit =
    b(at) = n.toByte
    b(at + 1) = (n >>> 8).toByte
  private def f32(b: Array[Byte], at: Int, value: Float): Unit =
    val bits = java.lang.Float.floatToRawIntBits(value)
    var i = 0
    while i < 4 do
      b(at + i) = (bits >>> (8 * i)).toByte
      i += 1

  test("raw metadata stays uninterpreted and payload uses first-axis-fastest order"):
    val raw = right(Nifti1Container.parse(IArray.unsafeFromArray(bytes)))
    assertEquals(raw.shape, header.dims)
    assertEquals(raw.intentP, header.intentP)
    assertEquals(raw.intentCode, 1007)
    assertEquals(raw.intentName, "raw-vector")
    assertEquals(raw.qformCode, 1)
    assertEquals(raw.sformCode, 2)
    assertEqualsDouble(raw.qformRowMajor(3), 5.0, 0.0)
    assertEqualsDouble(raw.sformRowMajor(3), 11.0, 0.0)
    assertEqualsDouble(raw.value(1, 2, 0, 0, 2), values(17), 0.0)
    val rewritten = right(Nifti1Container.parse(Nifti1Writer.write(Nifti1Writer.headerOf(raw), values)))
    assertEquals(rewritten.intentP, raw.intentP)
    assertEquals(rewritten.srow, raw.srow)
    assertEqualsDouble(rewritten.value(17L), values(17), 0.0)

  test("scaling applies once and zero slope ignores intercept"):
    val b = bytes
    f32(b, 112, 2.0f)
    f32(b, 116, -3.0f)
    assertEqualsDouble(right(Nifti1Container.parse(IArray.unsafeFromArray(b))).value(0L), -2.5, 0.0)
    f32(b, 112, 0.0f)
    assertEqualsDouble(right(Nifti1Container.parse(IArray.unsafeFromArray(b))).value(0L), 0.25, 0.0)

  test("inconsistent widths, offsets, truncated payloads and overflowing extents fail admission"):
    val badBits = bytes
    i16(badBits, 72, 8)
    assert(Nifti1Container.parse(IArray.unsafeFromArray(badBits)).isLeft)
    Vector(-1.0f, 348.0f, 352.5f, Float.NaN, Float.PositiveInfinity).foreach: offset =>
      val b = bytes
      f32(b, 108, offset)
      assert(Nifti1Container.parse(IArray.unsafeFromArray(b)).isLeft, s"offset $offset")
    assert(Nifti1Container.parse(IArray.unsafeFromArray(bytes.dropRight(1))).isLeft)
    val overflow = bytes
    i16(overflow, 40, 7)
    (1 to 7).foreach(i => i16(overflow, 40 + 2 * i, Short.MaxValue.toInt))
    assert(Nifti1Container.parse(IArray.unsafeFromArray(overflow)).isLeft)

  test("flat reads and writer metadata cannot escape admitted bounds"):
    val raw = right(Nifti1Container.parse(IArray.unsafeFromArray(bytes)))
    intercept[IllegalArgumentException](raw.value(-1L))
    intercept[IllegalArgumentException](raw.value(18L))
    intercept[IllegalArgumentException](header.copy(srow = Vector.empty))
    intercept[IllegalArgumentException](header.copy(pixdim = Vector.fill(8)(1.0)))
    intercept[IllegalArgumentException](header.copy(dims = Vector(32768)))

  test("writer refuses a dimension product that wraps Long to zero"):
    // 16384^5 = 2^70 samples. No data allocation is needed to exercise admission.
    val enormous = header.copy(dims = Vector.fill(5)(16384))
    intercept[IllegalArgumentException](Nifti1Writer.write(enormous, Array.emptyDoubleArray))
