package image4s.locus

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import locus4s.DomainRegistry
import locus4s.Region
import scala.compiletime.testing.typeCheckErrors

final class GridComponentsSuite extends munit.FunSuite:
  test("all 256 cube masks agree with an independent all-pairs union oracle"):
    val domain =
      right(GridDomain.register(grid(Vector(2, 2, 2)), "cube", DomainRegistry.empty)).value
    for
      bits <- 0 until 256
      connectivity <- GridConnectivity.values
    do
      val active = (0 until 8).filter(i => (bits & (1 << i)) != 0).toVector
      check(domain, active, connectivity)

  test("asymmetric and singleton axes preserve indexing and never wrap rows or planes"):
    for shape <- Vector(Vector(2, 3, 4), Vector(1, 3, 5), Vector(4, 1, 3), Vector(3, 4, 1)) do
      val domain = right(GridDomain.register(grid(shape), "asymmetric", DomainRegistry.empty)).value
      val masks = Vector(
        Vector.empty,
        Vector(0),
        (0 until domain.voxelCount).toVector,
        Vector(shape(2) - 1, shape(2)),
        Vector(shape(1) * shape(2) - 1, shape(1) * shape(2)).filter(_ < domain.voxelCount),
        (0 until domain.voxelCount).filter(i => i % 3 != 1).toVector
      )
      for
        active <- masks
        connectivity <- GridConnectivity.values
      do check(domain, active, connectivity)

  test("face, edge and corner contacts distinguish 6, 18 and 26 neighbors"):
    val domain =
      right(GridDomain.register(grid(Vector(2, 2, 2)), "contacts", DomainRegistry.empty)).value
    def count(active: Vector[Int], connectivity: GridConnectivity): Int =
      GridComponents
        .connected(
          domain,
          right(Region.fromOrdinals(domain.space, active)),
          connectivity
        )
        .size
    assertEquals(count(Vector(0, 3), GridConnectivity.Face), 2)
    assertEquals(count(Vector(0, 3), GridConnectivity.FaceEdge), 1)
    assertEquals(count(Vector(0, 7), GridConnectivity.FaceEdge), 2)
    assertEquals(count(Vector(0, 7), GridConnectivity.FaceEdgeCorner), 1)

  test("component order, member order and live owner are preserved"):
    val domain =
      right(GridDomain.register(grid(Vector(3, 4, 5)), "order", DomainRegistry.empty)).value
    val input = right(Region.fromOrdinals(domain.space, Vector(45, 19, 1, 24, 6, 40, 18, 0, 41, 5)))
    val before = input.ordinalsInDomainOrder.toVector
    val components = GridComponents.connected(domain, input)
    assertEquals(
      components.map(_.ordinalsInDomainOrder.toVector),
      Vector(
        Vector(0, 1, 5, 6),
        Vector(18, 19),
        Vector(24),
        Vector(40, 41, 45)
      )
    )
    assert(components.forall(_.space.sameRuntimeOwnerAs(domain.space)))
    assertEquals(input.ordinalsInDomainOrder.toVector, before)

  test("foreign domain owners and non-D3 grids are rejected by the public signature"):
    val ownerErrors = typeCheckErrors("""
      import image4s.geometry.{D3, Frame}
      import image4s.locus.{GridComponents, GridDomain}
      import locus4s.Region
      def foreign[F <: Frame[D3], S, T](domain: GridDomain[F, D3, S], active: Region[T]) =
        GridComponents.connected(domain, active)
    """)
    assert(ownerErrors.exists(_.message.contains("Region")))
    val dimensionErrors = typeCheckErrors("""
      import image4s.geometry.{D2, Frame}
      import image4s.locus.{GridComponents, GridDomain}
      import locus4s.Region
      def planar[F <: Frame[D2], S](domain: GridDomain[F, D2, S], active: Region[S]) =
        GridComponents.connected(domain, active)
    """)
    assert(dimensionErrors.exists(_.message.contains("D3")))

  private def check[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      active: Vector[Int],
      connectivity: GridConnectivity
  ): Unit =
    val region = right(Region.fromOrdinals(domain.space, active.reverse))
    val actual = GridComponents
      .connected(domain, region, connectivity)
      .map(_.ordinalsInDomainOrder.toVector)
    assertEquals(
      actual,
      oracle(domain.grid.shape, active, connectivity),
      s"${domain.grid.shape}, $active, $connectivity"
    )
    assertEquals(actual.flatten.sorted, active.sorted)

  // Build adjacency by all coordinate pairs, then union their equivalence classes. No flood fill,
  // neighbor-offset table, ordinal arithmetic or provider indexing helpers are used here.
  private def oracle(
      shape: Vector[Int],
      active: Vector[Int],
      connectivity: GridConnectivity
  ): Vector[Vector[Int]] =
    val coordinates = (for
      x <- 0 until shape(0)
      y <- 0 until shape(1)
      z <- 0 until shape(2)
    yield Vector(x, y, z)).toVector
    val labels = active.indices.toArray
    val maxChangedAxes = connectivity match
      case GridConnectivity.Face => 1
      case GridConnectivity.FaceEdge => 2
      case GridConnectivity.FaceEdgeCorner => 3
    for
      i <- active.indices
      j <- 0 until i
    do
      val differences =
        coordinates(active(i)).zip(coordinates(active(j))).map((a, b) => math.abs(a - b))
      if differences.max <= 1 && differences.count(_ != 0) <= maxChangedAxes then
        val oldLabel = labels(j)
        val newLabel = labels(i)
        for k <- labels.indices do if labels(k) == oldLabel then labels(k) = newLabel
    active.indices
      .groupBy(labels(_))
      .values
      .map(indices => indices.map(active).sorted.toVector)
      .toVector
      .sortBy(_.head)

  private def grid(shape: Vector[Int]): Grid[? <: Frame[D3], D3] =
    val frame = right(
      Frame.persistentNamed[D3](
        right(FrameId.parse("grid-components-frame")),
        "grid component fixture",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )
    right(
      Grid.createPersistent(right(GridId.parse("grid-components-grid")), frame)(
        shape,
        Affine.identity[D3]
      )
    )

  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
