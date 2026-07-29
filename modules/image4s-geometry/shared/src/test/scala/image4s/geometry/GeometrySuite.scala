package image4s.geometry

import scala.compiletime.testing.typeCheckErrors

final class GeometrySuite extends munit.FunSuite:
  test("affine andThen applies coordinate operators in declared order"):
    val translate =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            1.0, 0.0, 3.0,
            0.0, 1.0, -2.0,
            0.0, 0.0, 1.0
          )
        )
      )
    val scale =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            2.0, 0.0, 0.0,
            0.0, 4.0, 0.0,
            0.0, 0.0, 1.0
          )
        )
      )
    val composed = right(translate.andThen(scale))
    val transformed = right(composed(Vector(1.0, 5.0)))

    assertEquals(transformed, Vector(8.0, 12.0))

  test("same-named fresh frames have distinct persistent and runtime identity"):
    val first = right(Frame.named[D2]("same"))
    val second = right(Frame.named[D2]("same"))
    val point = right(Point.in[D2](first)(0.0, 0.0))

    assertNotEquals(first.id, second.id)
    assert(!point.belongsTo(second))
    assert(Frame.align(first, second).isLeft)

  test("registered frame restore recovers the exact live owner and evidence"):
    val registry = FrameRegistry.empty
    val frame = right(Frame.named[D3]("native"))
    right(registry.register(frame))
    val restored = right(Frame.restore[D3](frame.record, registry))
    val alignment = right(Frame.align(frame, restored))
    val point = right(Point.in[D3](frame)(1.0, 2.0, 3.0))
    val rebound = right(alignment.pointToRight(point))

    assert(frame eq restored)
    assert(alignment.sameRuntimeOwner)
    assert(rebound.belongsTo(restored))
    assertEquals(rebound.coordinates, point.coordinates)

  test("frame restore rejects dimension and metadata conflicts"):
    val registry = FrameRegistry.empty
    val frame = right(Frame.named[D2]("plane"))
    right(registry.register(frame))
    val wrongRank = frame.record.copy(spatialRank = 3)
    val otherMetadata = right(FrameMetadata.named("other"))
    val wrongMetadata = frame.record.copy(metadata = otherMetadata)

    assert(Frame.restore[D2](wrongRank, registry).isLeft)
    assert(Frame.restore[D2](wrongMetadata, registry).isLeft)

  test("alignment explicitly rebinds values across registry boundaries"):
    val source = right(Frame.named[D2]("serialized"))
    val first = right(Frame.restore[D2](source.record, FrameRegistry.empty))
    val second = right(Frame.restore[D2](source.record, FrameRegistry.empty))
    val point = right(Point.in[D2](first)(3.0, 4.0))
    val alignment = right(Frame.align(first, second))
    val rebound = right(alignment.pointToRight(point))

    assertEquals(first.id, second.id)
    assert(!alignment.sameRuntimeOwner)
    assert(rebound.belongsTo(second))
    assert(!rebound.belongsTo(first))
    assertEquals(rebound.coordinates, point.coordinates)

  test("widened values still diagnose distinct live frame owners"):
    val source = right(Frame.named[D2]("widened"))
    val first = right(Frame.restore[D2](source.record, FrameRegistry.empty))
    val second = right(Frame.restore[D2](source.record, FrameRegistry.empty))
    val point: Point[Frame[D2], D2] =
      right(Point.fromVector[D2, Frame[D2]](first, Vector(1.0, 2.0)))
    val vector: Vec[Frame[D2], D2] =
      right(Vec.fromVector[D2, Frame[D2]](second, Vector(3.0, 4.0)))

    (point + vector) match
      case Left(GeometryError.FrameOwnerMismatch(id)) =>
        assertEquals(id, source.id)
      case other =>
        fail(s"expected FrameOwnerMismatch, got $other")

  test("dynamic restore recovers a sealed dimension witness"):
    val record = right(Frame.named[D3]("dynamic")).record
    val restored = right(Frame.restoreDynamic(record, FrameRegistry.empty))

    assertEquals(restored.dimension.rank, 3)
    assertEquals(restored.value.spatialRank, 3)

  test("static dimensions reject D3 points in D2 frames"):
    val errors = typeCheckErrors(
      """
import image4s.geometry.*
val frame = Frame.named[D2]("plane").toOption.get
Point.in[D3](frame)(0.0, 0.0, 0.0)
"""
    )
    assert(errors.nonEmpty)

  test("callers cannot provide lying dimension evidence"):
    val errors = typeCheckErrors(
      """
import image4s.geometry.*
given Dimension[D2] with
  val rank = 3
"""
    )
    assert(errors.nonEmpty)

  test("FrameId and GridId are not interchangeable"):
    val errors = typeCheckErrors(
      """
import image4s.geometry.*
def requiresFrameId(value: FrameId): Unit = ()
val gridId = GridId.parse("grid").toOption.get
requiresFrameId(gridId)
"""
    )
    assert(errors.nonEmpty)

  test("affines reject invalid homogeneous rows and copy borrowed data"):
    val invalid = Affine.fromRowMajor[D2](
      Vector(
        1.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        1.0,
        1.0
      )
    )
    assert(invalid.isLeft)

    val borrowed =
      Array(1.0, 0.0, 4.0, 0.0, 1.0, 5.0, 0.0, 0.0, 1.0)
    val affine = right(Affine.fromRowMajor[D2](borrowed))
    borrowed(0) = 99.0
    borrowed(2) = -200.0

    assertEqualsDouble(affine.matrix(0, 0), 1.0, 0.0)
    assertEqualsDouble(affine.matrix(0, 2), 4.0, 0.0)

  test("affine tolerance is bounded and accepted bottom rows are canonicalized"):
    val projective = Vector(
      1.0,
      0.0,
      1.0,
      0.0,
      1.0,
      0.0,
      0.5,
      0.0,
      1.0
    )
    assert(Affine.fromRowMajor[D2](projective, tolerance = 0.5).isLeft)

    val nearlyAffine = projective.updated(6, 1e-13)
    val affine = right(
      Affine.fromRowMajor[D2](nearlyAffine, tolerance = 1e-12)
    )
    val recovered = right(affine.inverse(right(affine(Vector(2.0, 3.0)))))
    assertEquals(affine.rowMajor.takeRight(3), Vector(0.0, 0.0, 1.0))
    assertEqualsDouble(recovered(0), 2.0, 1e-10)
    assertEqualsDouble(recovered(1), 3.0, 1e-10)

  test("affine inverse round-trips non-axis-aligned physical coordinates"):
    val affine = right(
      Affine.fromOriginSpacingDirection[D2](
        origin = Vector(10.0, -3.0),
        spacing = Vector(2.0, 4.0),
        directionRowMajor = Vector(0.0, -1.0, 1.0, 0.0)
      )
    )
    val physical = right(affine(Vector(1.25, 2.5)))
    val recovered = right(affine.inverse(physical))

    assertEqualsDouble(recovered(0), 1.25, 1e-10)
    assertEqualsDouble(recovered(1), 2.5, 1e-10)

  test("multiple grids may share a frame while retaining distinct GridIds"):
    val frame = right(Frame.named[D2]("plane"))
    val first = right(Grid.in[D2](frame)(Vector(8, 9), Affine.identity[D2]))
    val second = right(Grid.in[D2](frame)(Vector(8, 9), Affine.identity[D2]))

    assertEquals(first.frame.id, second.frame.id)
    assertNotEquals(first.id, second.id)
    assert(Grid.align(first, second).isLeft)

  test("registered grid restore recovers exact owner and rejects conflicts"):
    val frame = right(Frame.named[D2]("plane"))
    val grid = right(Grid.in[D2](frame)(Vector(8, 9), Affine.identity[D2]))
    val registry = GridRegistry.empty
    right(registry.register(grid))
    val restored = right(Grid.restore(grid.record, frame, registry))
    val alignment = right(Grid.align(grid, restored))
    val point = right(grid.pointAt(right(Index.of[D2](2, 3))))
    val rebound = right(alignment.pointToRight(point))

    assert(grid eq restored)
    assert(alignment.sameRuntimeOwner)
    assert(rebound.belongsTo(restored.frame))
    assert(
      Grid
        .restore(grid.record.copy(shape = Vector(9, 8)), frame, registry)
        .isLeft
    )

  test("grid restore rejects a distinct live owner of the recorded frame"):
    val source = right(Frame.named[D2]("serialized-grid"))
    val firstFrame =
      right(Frame.restore[D2](source.record, FrameRegistry.empty))
    val secondFrame =
      right(Frame.restore[D2](source.record, FrameRegistry.empty))
    val grid =
      right(Grid.in[D2](firstFrame)(Vector(3, 4), Affine.identity[D2]))
    val registry = GridRegistry.empty
    right(registry.register(grid))

    val restored =
      Grid.restore(grid.record, secondFrame, registry)
    assert(
      restored.left.toOption.exists(
        _.isInstanceOf[GeometryError.GridRestoreFrameOwnerConflict]
      )
    )

  test("grids round-trip continuous indices through physical coordinates"):
    val frame = right(Frame.named[D2]("plane"))
    val affine = right(
      Affine.fromOriginSpacingDirection[D2](
        origin = Vector(10.0, -3.0),
        spacing = Vector(2.0, 4.0),
        directionRowMajor = Vector(0.0, -1.0, 1.0, 0.0)
      )
    )
    val grid = right(Grid.in[D2](frame)(Vector(8, 9), affine))
    val index = right(ContinuousIndex.of[D2](1.25, 2.5))
    val point = right(grid.pointAt(index))
    val recovered = right(grid.continuousIndexOf(point))

    assertEqualsDouble(recovered.values(0), 1.25, 1e-10)
    assertEqualsDouble(recovered.values(1), 2.5, 1e-10)

  test("point and vector operations preserve frame ownership"):
    val frame = right(Frame.named[D3]("native"))
    val point = right(Point.in[D3](frame)(1.0, 2.0, 3.0))
    val delta = right(Vec.in[D3](frame)(4.0, -2.0, 0.5))
    val moved = right(point + delta)
    val recovered = right(moved - point)

    assertEquals(moved.coordinates, Vector(5.0, 0.0, 3.5))
    assertEquals(recovered.coordinates, delta.coordinates)

  private def right[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def right(value: Either[GeometryError, Unit]): Unit =
    value match
      case Right(())   => ()
      case Left(error) => fail(error.message)
