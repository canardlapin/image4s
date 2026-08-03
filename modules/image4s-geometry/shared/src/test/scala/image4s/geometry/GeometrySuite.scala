package image4s.geometry

import scala.compiletime.testing.typeCheckErrors

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class GeometrySuite extends ScalaCheckSuite:
  property("affine identity and inverse round trip finite coordinates"):
    forAll(
      Gen.choose(-100.0, 100.0),
      Gen.choose(-100.0, 100.0),
      Gen.choose(0.1, 10.0),
      Gen.choose(0.1, 10.0),
      Gen.choose(-100.0, 100.0),
      Gen.choose(-100.0, 100.0)
    ): (tx, ty, sx, sy, x, y) =>
      val affine = affine2(tx, ty, sx, sy)
      val identity = Affine.identity[D2]
      val leftIdentity = right(identity.andThen(affine))
      val rightIdentity = right(affine.andThen(identity))
      val physical = right(affine(Vector(x, y)))
      val recovered = right(affine.inverse(physical))

      assertVectorClose(
        right(leftIdentity(Vector(x, y))),
        physical,
        1e-10
      )
      assertVectorClose(
        right(rightIdentity(Vector(x, y))),
        physical,
        1e-10
      )
      assertVectorClose(recovered, Vector(x, y), 1e-10)

  property("affine composition is associative"):
    forAll(
      Gen.choose(-10.0, 10.0),
      Gen.choose(-10.0, 10.0),
      Gen.choose(-10.0, 10.0),
      Gen.choose(-10.0, 10.0),
      Gen.choose(-10.0, 10.0)
    ): (firstShift, secondShift, thirdShift, x, y) =>
      val first = affine2(firstShift, 0.0, 1.25, 0.75)
      val second = affine2(0.0, secondShift, 0.8, 1.5)
      val third = affine2(thirdShift, -thirdShift, 1.1, 0.9)
      val left =
        right(right(first.andThen(second)).andThen(third))
      val rightAssociated =
        right(first.andThen(right(second.andThen(third))))

      assertVectorClose(
        right(left(Vector(x, y))),
        right(rightAssociated(Vector(x, y))),
        1e-10
      )

  property("persistent frame keys and records round trip"):
    forAll(
      Gen.choose(1, 1000000),
      Gen.oneOf(LengthUnit.values.toVector),
      Gen.oneOf(CoordinateConvention.values.toVector)
    ): (suffix, unit, convention) =>
      val id = frameId(s"frame-$suffix")
      val frame =
        right(
          Frame.persistentNamed[D3](
            id,
            s"subject-$suffix",
            unit,
            convention
          )
        )
      val record = right(frame.record)
      val resolution =
        right(Frame.restore[D3](record, FrameRegistry.empty))

      assertEquals(resolution.frame.persistentKey, Some(record.key))
      assertEquals(right(resolution.frame.record), record)
      assertEquals(resolution.registry.size, 1)
      assert(!resolution.frame.sameRuntimeOwnerAs(frame))
      assert(resolution.frame.samePersistentKeyAs(frame))

  property("persistent grid keys and records round trip"):
    forAll(
      Gen.choose(1, 32),
      Gen.choose(1, 32),
      Gen.choose(-100.0, 100.0),
      Gen.choose(-100.0, 100.0)
    ): (rows, columns, x, y) =>
      val frame =
        persistentFrame[D2](
          "frame-grid-property",
          "property-frame"
        )
      val affine =
        right(
          Affine.fromOriginSpacingDirection[D2](
            origin = Vector(x, y),
            spacing = Vector(1.5, 2.5),
            directionRowMajor = Vector(1.0, 0.0, 0.0, 1.0)
          )
        )
      val grid =
        right(
          Grid.createPersistent(
            gridId(s"grid-$rows-$columns-${x.toLong}-${y.toLong}"),
            frame
          )(Vector(rows, columns), affine)
        )
      val record = right(grid.record)
      val resolution =
        right(Grid.restore(record, frame, GridRegistry.empty))

      assertEquals(resolution.grid.persistentKey, Some(record.key))
      assertEquals(right(resolution.grid.record), record)
      assertEquals(resolution.registry.size, 1)
      assert(!resolution.grid.sameRuntimeOwnerAs(grid))
      assert(resolution.grid.samePersistentKeyAs(grid))

  property("bounded grid indices agree with every lattice boundary"):
    forAll(
      Gen.choose(1, 64),
      Gen.choose(1, 64),
      Gen.choose(Int.MinValue, Int.MaxValue),
      Gen.choose(Int.MinValue, Int.MaxValue)
    ): (rows, columns, row, column) =>
      val frame = right(Frame.named[D2]("index-property"))
      val grid =
        right(
          Grid.in(frame)(
            Vector(rows, columns),
            Affine.identity[D2]
          )
        )
      val lattice = right(LatticeIndex.of[D2](row, column))
      val expected =
        row >= 0 && row < rows && column >= 0 && column < columns

      assertEquals(grid.contains(lattice), expected)
      grid.index(lattice) match
        case Right(bounded) =>
          assert(expected)
          assert(bounded.grid eq grid)
          assertEquals(bounded.lattice.values, Vector(row, column))
          assert(right(grid.pointAt(bounded)).belongsTo(frame))
        case Left(_: GeometryError.GridIndexOutOfBounds) =>
          assert(!expected)
        case other =>
          fail(s"unexpected bounded-index result $other")

  test("ephemeral constructors perform no hidden persistent identity effect"):
    val first = right(Frame.named[D2]("same"))
    val second = right(Frame.named[D2]("same"))
    val firstGrid =
      right(Grid.in(first)(Vector(8, 9), Affine.identity[D2]))
    val secondGrid =
      right(Grid.in(first)(Vector(8, 9), Affine.identity[D2]))

    assertEquals(first.persistentKey, None)
    assertEquals(second.persistentKey, None)
    assertEquals(first.record, Left(GeometryError.EphemeralFrameHasNoRecord))
    assert(!first.sameRuntimeOwnerAs(second))
    assertEquals(
      Frame.align(first, second),
      Left(GeometryError.EphemeralFrameMismatch)
    )
    assertEquals(firstGrid.persistentKey, None)
    assertEquals(
      firstGrid.record,
      Left(GeometryError.EphemeralGridHasNoRecord)
    )
    assert(!firstGrid.sameRuntimeOwnerAs(secondGrid))
    assertEquals(
      Grid.align(firstGrid, secondGrid),
      Left(GeometryError.EphemeralGridMismatch)
    )
    assertEquals(
      FrameRegistry.empty.register(first),
      Left(GeometryError.CannotRegisterEphemeralFrame)
    )
    assertEquals(
      GridRegistry.empty.register(firstGrid),
      Left(GeometryError.CannotRegisterEphemeralGrid)
    )

  test("immutable frame registry restores one owner and ignores label changes"):
    val frame =
      persistentFrame[D3](
        "frame-native",
        "original",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    val empty = FrameRegistry.empty
    val registered = right(empty.register(frame))
    val renamedRecord =
      right(frame.record).copy(
        metadata = right(FrameMetadata.named("renamed"))
      )
    val restored =
      right(Frame.restore[D3](renamedRecord, registered))
    val alignment = right(Frame.align(frame, restored.frame))

    assertEquals(empty.size, 0)
    assertEquals(registered.size, 1)
    assert(restored.frame ne frame)
    assert(restored.frame.sameRuntimeOwnerAs(frame))
    assert(restored.registry eq registered)
    assertEquals(restored.frame.metadata.label, "original")
    assert(alignment.sameRuntimeOwner)
    assertEquals(right(restored.frame.record).key, renamedRecord.key)

  test("separate registries preserve distinct owners behind one frame key"):
    val source =
      persistentFrame[D2](
        "frame-serialized",
        "serialized",
        LengthUnit.Meter,
        CoordinateConvention.LPS
      )
    val record = right(source.record)
    val first =
      right(Frame.restore[D2](record, FrameRegistry.empty)).frame
    val second =
      right(Frame.restore[D2](record, FrameRegistry.empty)).frame
    val point = right(Point.in[D2](first)(3.0, 4.0))
    val alignment = right(Frame.align(first, second))
    val rebound = right(alignment.pointToRight(point))

    assert(first ne second)
    assert(!alignment.sameRuntimeOwner)
    assert(first.samePersistentKeyAs(second))
    assert(rebound.belongsTo(second))
    assert(!rebound.belongsTo(first))
    assertEquals(rebound.coordinates, point.coordinates)

  test("frame key conflicts fail closed for every structural field"):
    val id = frameId("frame-conflict")
    val frame =
      right(
        Frame.persistentNamed[D2](
          id,
          "registered",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val registered = right(FrameRegistry.empty.register(frame))
    val record = right(frame.record)
    val otherOwner =
      right(
        Frame.persistentNamed[D2](
          id,
          "other-owner",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )

    assertEquals(
      registered.register(otherOwner),
      Left(GeometryError.FrameRestoreDuplicateOwner(id))
    )
    assert(
      Frame
        .restore[D2](
          record.copy(
            key = record.key.copy(unit = LengthUnit.Meter)
          ),
          registered
        )
        .left
        .toOption
        .exists {
          case _: GeometryError.FrameKeyConflict => true
          case _ => false
        }
    )
    assert(
      Frame
        .restore[D2](
          record.copy(
            key = record.key.copy(
              convention = CoordinateConvention.LPS
            )
          ),
          registered
        )
        .left
        .toOption
        .exists {
          case _: GeometryError.FrameKeyConflict => true
          case _ => false
        }
    )
    assert(
      Frame
        .restore[D3](
          record.copy(
            key = record.key.copy(spatialRank = 3)
          ),
          registered
        )
        .left
        .toOption
        .exists {
          case _: GeometryError.FrameKeyConflict => true
          case _ => false
        }
    )

  test("dynamic restoration recovers dimension and immutable registry"):
    val source =
      persistentFrame[D3]("frame-dynamic", "dynamic")
    val (restored, registry) =
      right(
        Frame.restoreDynamic(
          right(source.record),
          FrameRegistry.empty
        )
      )

    assertEquals(restored.dimension.rank, 3)
    assertEquals(restored.value.spatialRank, 3)
    assertEquals(registry.size, 1)

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

  test("bounded grid indices reject every edge outside the finite grid"):
    val frame = right(Frame.named[D2]("bounded"))
    val grid =
      right(Grid.in(frame)(Vector(2, 3), Affine.identity[D2]))

    assertEquals(
      grid.index(-1, 0),
      Left(GeometryError.GridIndexOutOfBounds(0, -1, 2))
    )
    assertEquals(
      grid.index(2, 0),
      Left(GeometryError.GridIndexOutOfBounds(0, 2, 2))
    )
    assertEquals(
      grid.index(0, -1),
      Left(GeometryError.GridIndexOutOfBounds(1, -1, 3))
    )
    assertEquals(
      grid.index(0, 3),
      Left(GeometryError.GridIndexOutOfBounds(1, 3, 3))
    )
    assertEquals(
      grid.index(Int.MinValue, Int.MaxValue),
      Left(
        GeometryError.GridIndexOutOfBounds(
          0,
          Int.MinValue,
          2
        )
      )
    )
    val upper = right(grid.index(1, 2))
    assertEquals(upper.values, Vector(1, 2))

  test("bounded indices cannot cross grid owners at compile time"):
    val errors = typeCheckErrors(
      """
import image4s.geometry.*
val frame = Frame.named[D2]("plane").toOption.get
val first = Grid.in(frame)(Vector(2, 2), Affine.identity[D2]).toOption.get
val second = Grid.in(frame)(Vector(2, 2), Affine.identity[D2]).toOption.get
val index = first.index(1, 1).toOption.get
second.pointAt(index)
"""
    )
    assert(errors.nonEmpty)

  test("public point and vector constructors preserve singleton frame owners"):
    val positive = typeCheckErrors(
      """
import image4s.geometry.*
val frame = Frame.named[D2]("plane").toOption.get
val point: Point[frame.type, D2] =
  Point.fromVector(frame, Vector(1.0, 2.0)).toOption.get
val vector: Vec[frame.type, D2] =
  Vec.fromVector(frame, Vector(3.0, 4.0)).toOption.get
val moved: Point[frame.type, D2] = point + vector
val displacement: Vec[frame.type, D2] = moved - point
"""
    )
    assertEquals(positive, Nil)

    val widened = typeCheckErrors(
      """
import image4s.geometry.*
val frame = Frame.named[D2]("plane").toOption.get
Point.fromVector[D2, Frame[D2]](frame, Vector(1.0, 2.0))
"""
    )
    assert(widened.nonEmpty)

    val crossedOwners = typeCheckErrors(
      """
import image4s.geometry.*
val left = Frame.named[D2]("left").toOption.get
val right = Frame.named[D2]("right").toOption.get
val point = Point.in(left)(1.0, 2.0).toOption.get
val vector = Vec.in(right)(3.0, 4.0).toOption.get
point + vector
"""
    )
    assert(crossedOwners.nonEmpty)

  test("AffineIso documents equal intrinsic and ambient dimensions"):
    val errors = typeCheckErrors(
      """
import image4s.geometry.*
val iso: AffineIso[D3] = Affine.identity[D3]
val frame = Frame.named[D3]("volume").toOption.get
val grid: Grid[frame.type, D3] =
  Grid.in(frame)(Vector(2, 2, 1), iso).toOption.get
"""
    )
    assertEquals(errors, Nil)

  test("D2-in-D3 requires a future explicit embedding type"):
    val errors = typeCheckErrors(
      """
import image4s.geometry.*
val patient = Frame.named[D3]("patient").toOption.get
Grid.in[D2](patient)(Vector(64, 64), Affine.identity[D2])
"""
    )
    assert(errors.nonEmpty)

    val patient = right(Frame.named[D3]("patient-singleton"))
    val singleton =
      right(
        Grid.in(patient)(
          Vector(64, 64, 1),
          Affine.identity[D3]
        )
      )
    assertEquals(singleton.shape, Vector(64, 64, 1))

  test("affine andThen applies coordinate operators in declared order"):
    val translate =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            1.0, 0.0, 3.0, 0.0, 1.0, -2.0, 0.0, 0.0, 1.0
          )
        )
      )
    val scale =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            2.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 1.0
          )
        )
      )
    val composed = right(translate.andThen(scale))
    val transformed = right(composed(Vector(1.0, 5.0)))

    assertEquals(transformed, Vector(8.0, 12.0))

  test("affines reject invalid homogeneous rows and copy borrowed data"):
    val invalid = Affine.fromRowMajor[D2](
      Vector(
        1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0
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
      1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.5, 0.0, 1.0
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

  test("direction cosines enforce orthonormality and allow reflections"):
    val nonOrthonormal =
      Affine.fromOriginSpacingDirection[D2](
        origin = Vector(0.0, 0.0),
        spacing = Vector(2.0, 3.0),
        directionRowMajor = Vector(1.0, 0.5, 0.0, 1.0)
      )
    assert(
      nonOrthonormal.left.toOption.exists(
        _ match
          case _: GeometryError.NonOrthonormalDirection => true
          case _ => false
      )
    )

    val reflection =
      right(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(10.0, -4.0),
          spacing = Vector(2.0, 3.0),
          directionRowMajor = Vector(-1.0, 0.0, 0.0, 1.0)
        )
      )
    assertVectorClose(
      right(reflection(Vector(1.0, 2.0))),
      Vector(8.0, 2.0),
      0.0
    )

  test("affines expose residual and condition diagnostics and reject instability"):
    val stable =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            2.0, 0.2, 4.0, 0.1, 3.0, -2.0, 0.0, 0.0, 1.0
          )
        )
      )
    assert(stable.diagnostics.conditionEstimateInfinityNorm >= 1.0)
    assert(
      stable.diagnostics.inverseResidualInfinityNorm <=
        Affine.DefaultMaximumInverseResidual
    )
    val translated =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            1.0, 0.0, 1e15, 0.0, 1.0, -1e15, 0.0, 0.0, 1.0
          )
        )
      )
    assertEquals(
      translated.diagnostics.conditionEstimateInfinityNorm,
      1.0
    )

    val nearSingular =
      Affine.fromRowMajor[D2](
        Vector(
          1.0, 0.0, 0.0, 0.0, 1e-14, 0.0, 0.0, 0.0, 1.0
        )
      )
    assert(
      nearSingular.left.toOption.exists(
        _ match
          case _: GeometryError.IllConditionedAffine => true
          case _ => false
      )
    )
    assertEquals(
      Affine.fromRowMajor[D2](
        stable.rowMajor,
        maximumConditionNumber = 0.5
      ),
      Left(GeometryError.InvalidMaximumConditionNumber(0.5))
    )
    val strictResidual =
      Affine.fromRowMajor[D2](
        stable.rowMajor,
        maximumInverseResidual = 0.0
      )
    assert(
      strictResidual.left.toOption.exists(
        _ match
          case _: GeometryError.AffineInverseResidualTooLarge => true
          case _ => false
      )
    )

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

  test("persistent grids require persistent frames"):
    val frame = right(Frame.named[D2]("ephemeral"))
    val id = gridId("grid-requires-frame")

    assertEquals(
      Grid.createPersistent(id, frame)(
        Vector(2, 3),
        Affine.identity[D2]
      ),
      Left(GeometryError.PersistentGridRequiresPersistentFrame(id))
    )

  test("immutable grid registry restores one owner across tolerances"):
    val frame =
      persistentFrame[D2]("frame-grid-restore", "grid-frame")
    val id = gridId("grid-restore")
    val affine =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            1.0, 0.0, 2.0, 0.0, 1.0, 3.0, 0.0, 0.0, 1.0
          ),
          tolerance = 1e-10
        )
      )
    val grid =
      right(
        Grid.createPersistent(id, frame)(Vector(8, 9), affine)
      )
    val empty = GridRegistry.empty
    val registered = right(empty.register(grid))
    val restored =
      right(
        Grid.restore(
          right(grid.record),
          frame,
          registered,
          validationTolerance = 1e-6
        )
      )

    assertEquals(empty.size, 0)
    assertEquals(registered.size, 1)
    assert(restored.grid ne grid)
    assert(restored.grid.sameRuntimeOwnerAs(grid))
    assert(restored.registry eq registered)
    assertEquals(
      right(restored.grid.record).key,
      right(grid.record).key
    )
    val record = right(grid.record)
    val nonCanonical =
      record.key.indexToFrame.rowMajor.updated(6, 1e-13)
    assertEquals(
      Grid.restore(
        GridRecord(
          record.key.copy(
            indexToFrame = CanonicalAffineRecord(nonCanonical)
          )
        ),
        frame,
        registered,
        validationTolerance = 1e-6
      ),
      Left(GeometryError.NonCanonicalGridAffineRecord(id))
    )

  test("grid key conflicts cover frame, shape, affine, and duplicate owners"):
    val frame =
      persistentFrame[D2]("frame-grid-conflict", "registered-frame")
    val otherFrame =
      persistentFrame[D2]("frame-grid-other", "other-frame")
    val id = gridId("grid-conflict")
    val grid =
      right(
        Grid.createPersistent(id, frame)(
          Vector(3, 4),
          Affine.identity[D2]
        )
      )
    val record = right(grid.record)
    val registered = right(GridRegistry.empty.register(grid))
    val duplicate =
      right(
        Grid.createPersistent(id, frame)(
          Vector(3, 4),
          Affine.identity[D2]
        )
      )
    val changedAffine =
      record.key.indexToFrame.rowMajor.updated(2, 1.0)

    assertEquals(
      registered.register(duplicate),
      Left(GeometryError.GridRestoreDuplicateOwner(id))
    )
    assert(
      Grid
        .restore(
          GridRecord(record.key.copy(shape = Vector(4, 3))),
          frame,
          registered
        )
        .left
        .toOption
        .exists {
          case _: GeometryError.GridKeyConflict => true
          case _ => false
        }
    )
    assert(
      Grid
        .restore(
          GridRecord(
            record.key.copy(
              indexToFrame = CanonicalAffineRecord(changedAffine)
            )
          ),
          frame,
          registered
        )
        .left
        .toOption
        .exists {
          case _: GeometryError.GridKeyConflict => true
          case _ => false
        }
    )
    assert(
      Grid
        .restore(record, otherFrame, registered)
        .left
        .toOption
        .exists {
          case _: GeometryError.GridFrameKeyMismatch => true
          case _ => false
        }
    )

  test("grid restore refuses a distinct live owner of the same frame key"):
    val source =
      persistentFrame[D2]("frame-grid-owner", "serialized-grid")
    val frameRecord = right(source.record)
    val firstFrame =
      right(
        Frame.restore[D2](frameRecord, FrameRegistry.empty)
      ).frame
    val secondFrame =
      right(
        Frame.restore[D2](frameRecord, FrameRegistry.empty)
      ).frame
    val grid =
      right(
        Grid.createPersistent(
          gridId("grid-owner"),
          firstFrame
        )(Vector(3, 4), Affine.identity[D2])
      )
    val registry = right(GridRegistry.empty.register(grid))

    assert(
      Grid
        .restore(right(grid.record), secondFrame, registry)
        .left
        .toOption
        .exists(
          _ match
            case _: GeometryError.GridRestoreFrameOwnerConflict => true
            case _ => false
        )
    )

  test("grid identity and geometric congruence are separate relations"):
    val frame =
      persistentFrame[D2](
        "frame-congruence",
        "congruence",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    val identity = Affine.identity[D2]
    val first =
      right(
        Grid.createPersistent(gridId("grid-first"), frame)(
          Vector(8, 9),
          identity
        )
      )
    val second =
      right(
        Grid.createPersistent(gridId("grid-second"), frame)(
          Vector(8, 9),
          identity
        )
      )
    val shifted =
      right(
        Affine.fromRowMajor[D2](
          Vector(
            1.0, 0.0, 1e-9, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0
          )
        )
      )
    val approximate =
      right(
        Grid.createPersistent(gridId("grid-approximate"), frame)(
          Vector(8, 9),
          shifted
        )
      )

    assert(Grid.align(first, second).isLeft)
    val exact = right(Grid.exactCongruence(first, second))
    assert(exact.exact)
    assert(Grid.exactCongruence(first, approximate).isLeft)
    val near =
      right(
        Grid.approximateCongruence(
          first,
          approximate,
          tolerance = 1e-8
        )
      )
    assert(!near.exact)
    assertEquals(near.tolerance, 1e-8)
    assert(
      Grid
        .approximateCongruence(first, approximate, Double.NaN)
        .isLeft
    )

  test("same grid key across registries aligns distinct live owners"):
    val frame =
      persistentFrame[D2]("frame-grid-alignment", "alignment")
    val source =
      right(
        Grid.createPersistent(gridId("grid-alignment"), frame)(
          Vector(2, 2),
          Affine.identity[D2]
        )
      )
    val record = right(source.record)
    val first =
      right(Grid.restore(record, frame, GridRegistry.empty)).grid
    val second =
      right(Grid.restore(record, frame, GridRegistry.empty)).grid
    val alignment = right(Grid.align(first, second))

    assert(first ne second)
    assert(!alignment.sameRuntimeOwner)
    assert(first.samePersistentKeyAs(second))

  test("grids round-trip continuous indices through physical coordinates"):
    val frame = right(Frame.named[D2]("plane"))
    val affine =
      right(
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
    val moved = point + delta
    val recovered = moved - point

    assertEquals(moved.coordinates, Vector(5.0, 0.0, 3.5))
    assertEquals(recovered.coordinates, delta.coordinates)

  test("erased-owner point and vector operations remain checked"):
    val left = right(Frame.named[D2]("left-owner"))
    val rightFrame = right(Frame.named[D2]("right-owner"))
    val point = right(Point.in[D2](left)(1.0, 2.0))
    val otherPoint = right(Point.in[D2](rightFrame)(0.0, 0.0))
    val vector = right(Vec.in[D2](rightFrame)(3.0, 4.0))
    val leftVector = right(Vec.in[D2](left)(1.0, 1.0))

    assertEquals(
      point.addChecked(vector),
      Left(GeometryError.EphemeralFrameMismatch)
    )
    assertEquals(
      point.subtractChecked(otherPoint),
      Left(GeometryError.EphemeralFrameMismatch)
    )
    assertEquals(
      leftVector.addChecked(vector),
      Left(GeometryError.EphemeralFrameMismatch)
    )

  test("canonical geometry key fixture is identical on JVM and Scala.js"):
    val frame =
      persistentFrame[D2](
        "frame-fixture",
        "fixture",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    val grid =
      right(
        Grid.createPersistent(gridId("grid-fixture"), frame)(
          Vector(2, 3),
          right(
            Affine.fromRowMajor[D2](
              Vector(
                2.0, 0.0, 10.0, 0.0, 3.0, -4.0, 0.0, 0.0, 1.0
              )
            )
          )
        )
      )
    val encoded =
      encodeFrameKey(frame.persistentKey.getOrElse(fail("missing key"))) +
        "\n" +
        encodeGridKey(
          grid.persistentKey.getOrElse(fail("missing key"))
        )

    assertEquals(
      encoded,
      "frame-fixture|2|Millimeter|RAS\n" +
        "grid-fixture|frame-fixture|2|2,3|" +
        "4000000000000000,0000000000000000,4024000000000000," +
        "0000000000000000,4008000000000000,c010000000000000," +
        "0000000000000000,0000000000000000,3ff0000000000000"
    )
    assertEquals(fnv1a32(encoded), 4134681289L)

  private def persistentFrame[D <: Dim](
      rawId: String,
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using Dimension[D]): Frame[D] =
    right(
      Frame.persistentNamed[D](
        frameId(rawId),
        label,
        unit,
        convention
      )
    )

  private def frameId(value: String): FrameId =
    right(FrameId.parse(value))

  private def gridId(value: String): GridId =
    right(GridId.parse(value))

  private def encodeFrameKey(key: FrameKey): String =
    s"${key.id.value}|${key.spatialRank}|${key.unit}|${key.convention}"

  private def encodeGridKey(key: GridKey): String =
    s"${key.id.value}|${key.frame.id.value}|${key.spatialRank}|" +
      s"${key.shape.mkString(",")}|" +
      key.indexToFrame.rowMajor.map(encodeDouble).mkString(",")

  private def encodeDouble(value: Double): String =
    val unpadded =
      java.lang.Long.toHexString(
        java.lang.Double.doubleToRawLongBits(value)
      )
    "0" * (16 - unpadded.length) + unpadded

  private def fnv1a32(value: String): Long =
    var hash = 0x811c9dc5L
    var index = 0
    while index < value.length do
      hash ^= value.charAt(index).toLong
      hash = (hash * 0x01000193L) & 0xffffffffL
      index += 1
    hash

  private def affine2(
      tx: Double,
      ty: Double,
      sx: Double,
      sy: Double
  ): Affine[D2] =
    right(
      Affine.fromRowMajor[D2](
        Vector(
          sx,
          0.0,
          tx,
          0.0,
          sy,
          ty,
          0.0,
          0.0,
          1.0
        )
      )
    )

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      tolerance: Double
  ): Unit =
    assertEquals(actual.size, expected.size)
    actual.zip(expected).foreach { case (left, right) =>
      assertEqualsDouble(left, right, tolerance)
    }

  private def right[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
