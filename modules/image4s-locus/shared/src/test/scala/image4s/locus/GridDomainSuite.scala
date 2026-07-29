package image4s.locus

import locus4s.DomainId
import locus4s.DomainRecord
import locus4s.DomainRegistry
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridRegistry
import image4s.geometry.Index

final class GridDomainSuite extends munit.FunSuite:
  test("asymmetric grids use last-axis-fastest row-major ordinals"):
    val frame = right(Frame.named[D3]("asymmetric"))
    val grid =
      right(Grid.in[D3](frame)(Vector(2, 3, 4), Affine.identity[D3]))
    val registry = sequentialRegistry("asymmetric-domain")
    val resolved = right(GridDomain.fresh(grid, "voxels", registry))
    val bridge = resolved.value

    assertEquals(bridge.voxelCount, 24)
    assertEquals(bridge.space.size, 24)
    assertEquals(right(bridge.ordinalOf(index3(0, 0, 0))), 0)
    assertEquals(right(bridge.ordinalOf(index3(0, 0, 1))), 1)
    assertEquals(right(bridge.ordinalOf(index3(0, 1, 0))), 4)
    assertEquals(right(bridge.ordinalOf(index3(1, 0, 0))), 12)
    assertEquals(right(bridge.ordinalOf(index3(1, 2, 3))), 23)

    for
      first <- 0 until 2
      second <- 0 until 3
      third <- 0 until 4
    do
      val index = index3(first, second, third)
      val ordinal = right(bridge.ordinalOf(index))
      val recovered = right(bridge.indexOfOrdinal(ordinal))
      val point = right(bridge.pointAt(index))
      val recoveredFromPoint = right(bridge.indexOf(point))

      assertEquals(recovered.values, index.values)
      assertEquals(recoveredFromPoint.values, index.values)

  test("indices and ordinals are checked at the bridge boundary"):
    val frame = right(Frame.named[D3]("bounds"))
    val grid =
      right(Grid.in[D3](frame)(Vector(2, 3, 4), Affine.identity[D3]))
    val resolved =
      right(
        GridDomain.fresh(
          grid,
          "bounds",
          sequentialRegistry("bounds-domain")
        )
      )
    val bridge = resolved.value

    assertEquals(
      bridge.ordinalOf(index3(-1, 0, 0)),
      Left(GridDomainError.SpatialIndexOutOfBounds(0, -1, 2))
    )
    assertEquals(
      bridge.ordinalOf(index3(0, 3, 0)),
      Left(GridDomainError.SpatialIndexOutOfBounds(1, 3, 3))
    )
    assertEquals(
      bridge.indexOfOrdinal(-1),
      Left(GridDomainError.OrdinalOutOfBounds(-1, 24))
    )
    assertEquals(
      bridge.indexOfOrdinal(24),
      Left(GridDomainError.OrdinalOutOfBounds(24, 24))
    )

  test("attachment validates domain size and overflow-safe voxel counts"):
    val frame = right(Frame.named[D2]("sizes"))
    val grid =
      right(Grid.in[D2](frame)(Vector(2, 3), Affine.identity[D2]))
    val wrongRecord =
      right(DomainRecord.parse("wrong-size", "wrong", 5))
    val wrongResolution =
      right(DomainRegistry.empty.restore(wrongRecord))
    val wrongSpace = wrongResolution.space

    assertEquals(
      GridDomain.attach(grid, wrongSpace),
      Left(GridDomainError.DomainSizeMismatch(6, 5))
    )

    val huge =
      right(
        Grid.in[D2](frame)(
          Vector(50000, 50000),
          Affine.identity[D2]
        )
      )
    assertEquals(
      GridDomain.attach(huge, wrongSpace),
      Left(
        GridDomainError.VoxelCountOverflow(
          Vector(50000, 50000),
          Int.MaxValue
        )
      )
    )

    assertEquals(
      Grid.in[D2](frame)(Vector(2, 0), Affine.identity[D2]),
      Left(GeometryError.NonPositiveGridExtent(1, 0))
    )

  test("records restore checked persistent evidence and registry ownership"):
    val frame = right(Frame.named[D2]("restore"))
    val grid =
      right(Grid.in[D2](frame)(Vector(3, 5), Affine.identity[D2]))
    val fresh =
      right(
        GridDomain.fresh(
          grid,
          "restored-domain",
          sequentialRegistry("restore-domain")
        )
      )
    val record = fresh.value.record
    val sameOwner =
      right(GridDomain.restore(record, grid, fresh.registry))
    val otherOwner =
      right(GridDomain.restore(record, grid, DomainRegistry.empty))

    assertEquals(sameOwner.value.record, record)
    assert(fresh.value.validateSpace(sameOwner.value.space).isRight)
    assert(!fresh.value.space.sameRuntimeOwnerAs(otherOwner.value.space))
    assert(
      fresh.value
        .validateSpace(otherOwner.value.space)
        .left
        .toOption
        .exists(
          _.isInstanceOf[GridDomainError.DomainRuntimeOwnerMismatch]
        )
    )

  test("equal grid records with distinct live owners remain distinct"):
    val frame = right(Frame.named[D2]("grid-owners"))
    val source =
      right(Grid.in[D2](frame)(Vector(2, 7), Affine.identity[D2]))
    val first =
      right(Grid.restore(source.record, frame, GridRegistry.empty))
    val second =
      right(Grid.restore(source.record, frame, GridRegistry.empty))
    val firstBridge =
      right(
        GridDomain.fresh(
          first,
          "first-grid-owner",
          sequentialRegistry("grid-owner-domain")
        )
      )

    assert(
      firstBridge.value
        .validateGrid(second)
        .left
        .toOption
        .contains(GridDomainError.GridRuntimeOwnerMismatch(source.id))
    )

    val rebound =
      right(
        GridDomain.restore(
          firstBridge.value.record,
          second,
          DomainRegistry.empty
        )
      )
    assert(rebound.value.validateGrid(second).isRight)
    assert(
      rebound.value
        .validateGrid(first)
        .left
        .toOption
        .contains(GridDomainError.GridRuntimeOwnerMismatch(source.id))
    )

  test("restore rejects mismatched grid and domain metadata"):
    val frame = right(Frame.named[D2]("record-mismatch"))
    val grid =
      right(Grid.in[D2](frame)(Vector(3, 4), Affine.identity[D2]))
    val fresh =
      right(
        GridDomain.fresh(
          grid,
          "record",
          sequentialRegistry("record-domain")
        )
      )
    val record = fresh.value.record
    val wrongGridRecord =
      record.copy(grid = record.grid.copy(shape = Vector(4, 3)))
    val wrongDomainId = right(DomainId.parse("wrong-domain"))
    val wrongDomain =
      right(DomainRecord.make(wrongDomainId, "wrong", 11))

    assert(
      GridDomain
        .restore(wrongGridRecord, grid, DomainRegistry.empty)
        .left
        .toOption
        .exists(_.isInstanceOf[GridDomainError.GridRecordMismatch])
    )
    assertEquals(
      GridDomain.restore(
        record.copy(domain = wrongDomain),
        grid,
        DomainRegistry.empty
      ),
      Left(GridDomainError.DomainSizeMismatch(12, 11))
    )

  private def index3(
      first: Int,
      second: Int,
      third: Int
  ): Index[D3] =
    right(Index.of[D3](first, second, third))

  private def sequentialRegistry(prefix: String): DomainRegistry =
    right(DomainRegistry.withSequentialIds(prefix))

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) =>
        result
      case Left(error) =>
        fail(s"expected Right, found Left($error)")
