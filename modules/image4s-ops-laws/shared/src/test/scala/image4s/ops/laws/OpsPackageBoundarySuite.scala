package image4s.ops.laws

import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

final class OpsPackageBoundarySuite extends FunSuite:
  test("ops packages are present and distinct from core representation APIs"):
    assertEquals(image4s.ops.OpError.InvalidArgument("x").message, "x")
    assertEquals(image4s.filter.FilterModule.name, "image4s-filter")
    assertEquals(
      image4s.morphology.MorphologyModule.name,
      "image4s-morphology"
    )
    val missingCoreDump = typeCheckErrors(
      """
import image4s.ops.RasterImage
"""
    )
    val missingIntaglio = typeCheckErrors(
      """
import image4s.filter.DisplayPlan
"""
    )
    assert(missingCoreDump.nonEmpty)
    assert(missingIntaglio.nonEmpty)

  test("format and locus workflow types stay outside ops namespaces"):
    val niftiErrors = typeCheckErrors("import image4s.ops.Nifti")
    val locusErrors = typeCheckErrors("import image4s.filter.GridDomain")
    val registrationErrors =
      typeCheckErrors("import image4s.morphology.Registration")
    assert(niftiErrors.nonEmpty)
    assert(locusErrors.nonEmpty)
    assert(registrationErrors.nonEmpty)

  test("core factories remain the construction surface for Sampled"):
    val errors = typeCheckErrors(
      """
import image4s.ops.Sampled
"""
    )
    assert(errors.nonEmpty)
