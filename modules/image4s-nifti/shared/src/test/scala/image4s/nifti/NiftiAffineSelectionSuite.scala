package image4s.nifti

import image4s.geometry.{Affine, D3}

class NiftiAffineSelectionSuite extends munit.FunSuite:
  private def right[A](result: Either[?, A]): A =
    result.fold(error => fail(error.toString), identity)

  private def translated(x: Double): Affine[D3] =
    right(Affine.fromRowMajor[D3](Vector(
      1.0, 0.0, 0.0, x,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    )))

  private val q = translated(1.0)
  private val s = translated(4.0)
  private val fallback = translated(7.0)
  private val header = NiftiHeader(
    dimensions = Vector(2, 3, 4),
    pixelDimensions = Vector(1.0, 1.0, 1.0),
    storedFourthAxisStep = None,
    datatype = NiftiDatatype.Float64,
    voxelOffset = 352,
    slope = 1.0,
    intercept = 0.0,
    qformCode = 1,
    qform = Some(q),
    sformCode = 1,
    sform = Some(s),
    fallbackAffine = fallback,
    byteOrder = NiftiByteOrder.LittleEndian,
    spatialUnit = NiftiSpatialUnit.Millimeter,
    temporalUnit = NiftiTemporalUnit.Unknown,
    temporalOrigin = right(NiftiTemporalOrigin.create(0.0)),
    storage = NiftiStorage.SingleFile,
    extensions = Vector.empty
  )

  test("header-only preference preserves the exact affine and disagreement diagnostic"):
    val selectedS = right(Nifti.selectAffine(header, NiftiAffinePolicy.PreferSform))
    val selectedQ = right(Nifti.selectAffine(header, NiftiAffinePolicy.PreferQform))
    assertEquals(selectedS.source, NiftiAffineSource.Sform)
    assertEquals(selectedQ.source, NiftiAffineSource.Qform)
    assert(selectedS.affine eq s)
    assert(selectedQ.affine eq q)
    selectedS.diagnostics match
      case Vector(NiftiDiagnostic.QformSformDisagreement(difference)) =>
        assertEqualsDouble(difference, 3.0, 0.0)
      case other => fail(s"expected exact disagreement, found $other")

  test("header-only selection uses the other form or fallback when needed"):
    val noS = header.copy(sformCode = 0, sform = None)
    val noQ = header.copy(qformCode = 0, qform = None)
    assert(right(Nifti.selectAffine(noS, NiftiAffinePolicy.PreferSform)).affine eq q)
    assert(right(Nifti.selectAffine(noQ, NiftiAffinePolicy.PreferQform)).affine eq s)
    val neither = noS.copy(qformCode = 0, qform = None)
    for policy <- Vector(NiftiAffinePolicy.PreferSform, NiftiAffinePolicy.PreferQform, NiftiAffinePolicy.RequireAgreement(0.0)) do
      val result = right(Nifti.selectAffine(neither, policy))
      assertEquals(result.source, NiftiAffineSource.Fallback)
      assert(result.affine eq fallback)
      assertEquals(result.diagnostics, Vector.empty)

  test("agreement tolerance includes its boundary and rejects disagreement"):
    assert(right(Nifti.selectAffine(header, NiftiAffinePolicy.RequireAgreement(3.0))).affine eq s)
    Nifti.selectAffine(header, NiftiAffinePolicy.RequireAgreement(2.0)) match
      case Left(NiftiError.AffineFormsDisagree(difference, tolerance)) =>
        assertEqualsDouble(difference, 3.0, 0.0)
        assertEqualsDouble(tolerance, 2.0, 0.0)
      case other => fail(s"expected disagreement refusal, found $other")
    for invalid <- Vector(-1.0, Double.NaN, Double.PositiveInfinity) do
      Nifti.selectAffine(header, NiftiAffinePolicy.RequireAgreement(invalid)) match
        case Left(_: NiftiError.InvalidAffineAgreementTolerance) => ()
        case other => fail(s"expected invalid tolerance refusal, found $other")

  test("explicit geometry is retained without substituting either stored form"):
    val explicit = translated(12.0)
    val selected = right(Nifti.selectAffine(header, NiftiAffinePolicy.UseExplicit(explicit)))
    assertEquals(selected.source, NiftiAffineSource.Explicit)
    assert(selected.affine eq explicit)
