package image4s.locus.consumer

import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

final class PackageBoundarySuite extends FunSuite:
  test("package membership cannot forge bridge values or resolutions"):
    val bridgeErrors = typeCheckErrors(
      """
        import image4s.locus.*
        import locus4s.*
        import image4s.geometry.*
        def forge[F <: Frame[D], D <: Dim, S](
          grid: Grid[F, D],
          space: FiniteSpace[S]
        ): GridDomain[F, D, S] =
          new GridDomain(grid, space, space.size)
      """
    )
    val resolutionErrors = typeCheckErrors(
      """
        import image4s.locus.*
        import locus4s.*
        import image4s.geometry.*
        def forge[F <: Frame[D], D <: Dim, A](
          suppliedRegistry: DomainRegistry,
          bridge: GridDomain[F, D, A]
        ): GridDomainResolution[F, D] =
          new GridDomainResolution[F, D]:
            type S = A
            val registry: DomainRegistry = suppliedRegistry
            val value: GridDomain[F, D, A] = bridge
      """
    )

    assert(bridgeErrors.nonEmpty)
    assert(resolutionErrors.nonEmpty)

  test("dimension-mismatched indices are rejected statically"):
    val errors = typeCheckErrors(
      """
        import image4s.locus.*
        import locus4s.*
        import image4s.geometry.*
        def invalid[F <: Frame[D2], S](
          bridge: GridDomain[F, D2, S],
          index: Index[D3]
        ): Unit =
          bridge.ordinalOf(index)
      """
    )

    assert(errors.nonEmpty)

  test("high-level locus workflows are absent from the bridge namespace"):
    val parcellationErrors = typeCheckErrors(
      "import image4s.locus.Parcellation"
    )
    val searchlightErrors = typeCheckErrors(
      "import image4s.locus.Searchlight"
    )
    val aggregationErrors = typeCheckErrors(
      "import image4s.locus.Aggregation"
    )

    assert(parcellationErrors.nonEmpty)
    assert(searchlightErrors.nonEmpty)
    assert(aggregationErrors.nonEmpty)
