package image4s.locus

import image4s.Axis
import image4s.AxisKind
import image4s.AxisUnit
import image4s.LatticeMap
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.GridRecord
import image4s.geometry.LatticeIndex
import image4s.geometry.LengthUnit
import image4s.locus.laws.GridDomainLaws
import locus4s.CenteredNeighborhoodSystem
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.PartialSurjection
import locus4s.Region
import locus4s.Relation
import locus4s.Selection
import locus4s.data.Aggregation
import locus4s.data.Field
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import ravel.DType.given
import ravel.NDArray

final class GridDomainSuite extends ScalaCheckSuite:
  test("asymmetric grids use versioned last-axis-fastest ordinals"):
    val grid = persistentGrid3("asymmetric", Vector(2, 3, 4))
    val bridge = register(grid, "voxels").value

    assertEquals(bridge.layout, GridDomain.Layout)
    assertEquals(bridge.voxelCount, 24)
    assertEquals(right(bridge.ordinalOf(index3(0, 0, 0))), 0)
    assertEquals(right(bridge.ordinalOf(index3(0, 0, 1))), 1)
    assertEquals(right(bridge.ordinalOf(index3(0, 1, 0))), 4)
    assertEquals(right(bridge.ordinalOf(index3(1, 0, 0))), 12)
    assertEquals(right(bridge.ordinalOf(index3(1, 2, 3))), 23)

    for ordinal <- 0 until bridge.voxelCount do
      assert(GridDomainLaws.ordinalIndexRoundTrip(bridge, ordinal))
      val index = right(bridge.indexOfOrdinal(ordinal))
      val domainIndex = right(bridge.domainIndexAt(index))
      assertEquals(right(bridge.indexOf(domainIndex)).values, index.values)

  property("D2 ordinal/index conversion is a mutual inverse"):
    forAll(Gen.choose(1, 19), Gen.choose(1, 17)): (first, second) =>
      val grid =
        persistentGrid2(s"property-d2-$first-$second", Vector(first, second))
      val bridge = register(grid, "property voxels").value
      for ordinal <- 0 until bridge.voxelCount do
        assert(GridDomainLaws.ordinalIndexRoundTrip(bridge, ordinal))

  property("D3 ordinal/index conversion is a mutual inverse"):
    forAll(
      Gen.choose(1, 8),
      Gen.choose(1, 7),
      Gen.choose(1, 6)
    ): (first, second, third) =>
      val grid =
        persistentGrid3(
          s"property-d3-$first-$second-$third",
          Vector(first, second, third)
        )
      val bridge = register(grid, "property voxels").value
      for ordinal <- 0 until bridge.voxelCount do
        assert(GridDomainLaws.ordinalIndexRoundTrip(bridge, ordinal))

  test("indices and ordinals fail closed at the bridge boundary"):
    val bridge =
      register(persistentGrid3("bounds", Vector(2, 3, 4)), "bounds").value

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

  test("canonical keys are deterministic and presentation-independent"):
    val grid = persistentGrid2("canonical", Vector(2, 3))
    val first = right(GridDomain.canonicalDomainRecord(grid, "first label"))
    val second =
      right(GridDomain.canonicalDomainRecord(grid, "renamed label"))

    assertEquals(first.key, second.key)
    assertNotEquals(first.metadata, second.metadata)
    assertEquals(
      first.id.value,
      "image4s:grid-domain:row-major-last-axis-fastest/v1:grid-canonical"
    )
    assertEquals(
      first.fingerprint.map(_.value),
      Some(
        "schema:26:image4s-grid-domain-key/v1|" +
          "layout:30:row-major-last-axis-fastest/v1|" +
          "grid-id:14:grid-canonical|" +
          "frame-id:15:frame-canonical|" +
          "rank:1:2|unit:10:Millimeter|convention:3:RAS|" +
          "shape:3:2,3|" +
          "affine-bits:152:" +
          "3ff0000000000000,0000000000000000,0000000000000000," +
          "0000000000000000,3ff0000000000000,0000000000000000," +
          "0000000000000000,0000000000000000,3ff0000000000000"
      )
    )

  test("one registry converges repeated registration on one live owner"):
    val grid = persistentGrid2("converge", Vector(3, 5))
    val first = register(grid, "initial")
    val second =
      right(GridDomain.register(grid, "renamed", first.registry))

    assert(first.value.space.sameRuntimeOwnerAs(second.value.space))
    assertEquals(first.value.space.key, second.value.space.key)
    assertEquals(second.registry.size, 1)

  test("same domain id with different grid structure fails as a collision"):
    val frame = persistentFrame[D2]("collision")
    val id = gridId("grid-collision")
    val first =
      right(
        Grid.createPersistent(id, frame)(
          Vector(2, 3),
          Affine.identity[D2]
        )
      )
    val second =
      right(
        Grid.createPersistent(id, frame)(
          Vector(3, 2),
          Affine.identity[D2]
        )
      )
    val registered = register(first, "first")

    assert(
      GridDomain
        .register(second, "second", registered.registry)
        .left
        .toOption
        .exists(_.isInstanceOf[GridDomainError.DomainRestoreFailure])
    )

  test("attachment rejects arbitrary same-sized domains and ephemeral grids"):
    val persistent = persistentGrid2("attach", Vector(2, 3))
    val unrelated =
      right(
        DomainRecord.parse(
          "unrelated",
          "unrelated",
          6,
          Some("unrelated-structure")
        )
      )
    val unrelatedSpace =
      right(DomainRegistry.empty.restore(unrelated)).space

    assert(
      GridDomain
        .attach(persistent, unrelatedSpace)
        .left
        .toOption
        .exists(_.isInstanceOf[GridDomainError.DomainKeyMismatch])
    )

    val ephemeralFrame = right(Frame.named[D2]("ephemeral"))
    val ephemeral =
      right(
        Grid.in(ephemeralFrame)(Vector(2, 3), Affine.identity[D2])
      )
    assert(
      GridDomain
        .canonicalDomainKey(ephemeral)
        .left
        .toOption
        .exists(_.isInstanceOf[GridDomainError.PersistentGridRequired])
    )

  test("voxel-count overflow is detected before domain construction"):
    val huge =
      persistentGrid2("overflow", Vector(50000, 50000))
    assertEquals(
      GridDomain.canonicalDomainKey(huge),
      Left(
        GridDomainError.VoxelCountOverflow(
          Vector(50000, 50000),
          Int.MaxValue
        )
      )
    )

  test("restore converges in one registry and preserves distinct live owners"):
    val grid = persistentGrid2("restore", Vector(3, 5))
    val fresh = register(grid, "restored")
    val sameOwner =
      right(GridDomain.restore(fresh.value.record, grid, fresh.registry))
    val otherOwner =
      right(
        GridDomain.restore(
          fresh.value.record,
          grid,
          DomainRegistry.empty
        )
      )

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

  test("equal persistent grids with different live owners require rebinding"):
    val source = persistentGrid2("grid-owners", Vector(2, 7))
    val record = right(source.record)
    val first =
      right(
        Grid.restore(record, source.frame, Grid.Registry.empty)
      ).grid
    val second =
      right(
        Grid.restore(record, source.frame, Grid.Registry.empty)
      ).grid
    val firstBridge = register(first, "first owner")

    assertEquals(
      firstBridge.value.validateGrid(second),
      Left(
        GridDomainError.GridRuntimeOwnerMismatch(
          gridId("grid-grid-owners")
        )
      )
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
    assert(rebound.value.validateGrid(first).isLeft)

  test("restore rejects grid, domain-key, and layout divergence"):
    val grid = persistentGrid2("record-mismatch", Vector(3, 4))
    val fresh = register(grid, "record")
    val record = fresh.value.record
    val wrongGrid =
      GridRecord(
        record.grid.key.copy(shape = Vector(4, 3))
      )
    val wrongDomain =
      right(
        DomainRecord.parse(
          record.domain.id.value,
          "wrong",
          record.domain.size,
          Some("wrong-fingerprint")
        )
      )

    assert(
      GridDomain
        .restore(
          record.copy(grid = wrongGrid),
          grid,
          DomainRegistry.empty
        )
        .left
        .toOption
        .exists(_.isInstanceOf[GridDomainError.GridRecordMismatch])
    )
    assert(
      GridDomain
        .restore(
          record.copy(domain = wrongDomain),
          grid,
          DomainRegistry.empty
        )
        .left
        .toOption
        .exists(_.isInstanceOf[GridDomainError.DomainKeyMismatch])
    )
    assertEquals(
      GridDomain.restore(
        record.copy(
          layout = GridDomainLayout.Unrecognized("future-layout/v9")
        ),
        grid,
        DomainRegistry.empty
      ),
      Left(
        GridDomainError.LayoutMismatch(
          GridDomain.Layout,
          GridDomainLayout.Unrecognized("future-layout/v9")
        )
      )
    )

  test("spatial fields are zero-copy and preserve row-major lookup"):
    val grid = persistentGrid2("spatial-field", Vector(2, 3))
    val bridge = register(grid, "spatial voxels").value
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 3)((i, j) => 10.0 * i + j)
        )
      )
    val field = right(bridge.spatialField(image))

    assert(GridDomainLaws.sharesImageStorage(field))
    assert(field.sourceData eq image.data)
    assertEquals(
      field(right(bridge.space.index(5))),
      12.0
    )

    val selection =
      right(Selection.fromOrdinals(bridge.space, Vector(4, 1, 5)))
    assert(GridDomainLaws.selectionGatherFollowsOrder(field, selection))
    assertEquals(field.gather(selection).toVector, Vector(11.0, 1.0, 12.0))

  test("series fields expose zero-copy non-spatial series per voxel"):
    val grid = persistentGrid2("series-field", Vector(2, 3))
    val bridge = register(grid, "series voxels").value
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          4,
          0.0,
          0.8,
          AxisUnit.Seconds
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Double](2, 3, 4)((i, j, t) => 100.0 * i + 10.0 * j + t)
        )
      )
    val field = right(bridge.seriesField(image))
    val series = field(right(bridge.space.index(4)))

    assert(field.sourceData eq image.data)
    assert(series.sourceData eq image.data)
    assertEquals(series.nonSpatialAxes.records, axes.records)
    assertEquals(imageRight(series.valueAt(Vector(3))), 113.0)
    assert(
      bridge
        .spatialField(image)
        .left
        .toOption
        .contains(
          GridDomainError.SpatialFieldRequiresNoNonSpatialAxes(1)
        )
    )

  test("exact crop commutes with image values, masks, and selections"):
    val grid = persistentGrid2("exact-crop", Vector(4, 5))
    val bridge = register(grid, "source voxels").value
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](4, 5)((i, j) => 10.0 * i + j)
        )
      )
    val sourceField = right(bridge.spatialField(image))
    val map =
      imageRight(
        LatticeMap.crop[D2](
          sourceShape = Vector(4, 5),
          origin = Vector(1, 1),
          targetShape = Vector(2, 3)
        )
      )
    val view = right(bridge.exactView(map))
    val targetImage = imageRight(image.view(map))
    val targetField =
      Field.view(view.target): targetIndex =>
        val coordinates =
          coordinatesOfOrdinal(map.targetShape, targetIndex.ordinal)
        imageRight(targetImage.valueAt(coordinates))

    assert(
      GridDomainLaws.exactViewCommutesWithField(
        sourceField,
        view
      )(targetField)
    )
    assertEquals(
      view.injection.toTotalMap.targetOrdinals.toVector,
      Vector(6, 7, 8, 11, 12, 13)
    )
    assertEquals(view.injection.support.cardinality, 6)
    assertEquals(view.bijection, None)

    val mask =
      right(Region.fromOrdinals(bridge.space, Vector(6, 8, 12, 19)))
    val pulled = view.injection.toTotalMap.pullback(mask)
    assertEquals(pulled.cardinality, 3)
    assertEquals(pulled.ordinalsInDomainOrder.toVector, Vector(0, 2, 4))

  test("exact views feed locus4s partitions and compact neighborhoods"):
    val grid = persistentGrid2("locus-algebra", Vector(4, 5))
    val bridge = register(grid, "source voxels").value
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](4, 5)((i, j) => 10.0 * i + j)
        )
      )
    val field = right(bridge.spatialField(image))
    val map =
      imageRight(
        LatticeMap.crop[D2](
          sourceShape = Vector(4, 5),
          origin = Vector(1, 1),
          targetShape = Vector(2, 3)
        )
      )
    val view = right(bridge.exactView(map))

    val optionalTargets = Array.fill[Option[Int]](bridge.voxelCount)(None)
    view.target.foreachIndex: center =>
      optionalTargets(view.injection(center).ordinal) = Some(center.ordinal)
    val partition =
      right(
        PartialSurjection.fromOptionalTargetOrdinals(
          bridge.space,
          view.target,
          optionalTargets
        )
      )
    val fibers = right(partition.fibers)

    assertEquals(
      partition.support.ordinalsInDomainOrder.toVector,
      Vector(6, 7, 8, 11, 12, 13)
    )
    assertEquals(
      view.target.indices
        .map(fibers.row)
        .map(_.ordinalsInDomainOrder.toVector)
        .toVector,
      Vector(Vector(6), Vector(7), Vector(8), Vector(11), Vector(12), Vector(13))
    )
    assertEquals(
      Aggregation
        .pushForward(partition, field)(0.0)(identity)(_ + _)
        .toVector,
      Vector(11.0, 12.0, 13.0, 21.0, 22.0, 23.0)
    )

    val membership =
      right(
        Relation.fromOrdinalRows(
          view.target,
          bridge.space,
          view.target.indices.map: center =>
            Vector(view.injection(center).ordinal)
        )
      )
    val neighborhoods =
      right(CenteredNeighborhoodSystem.from(view.injection, membership))

    assert(neighborhoods.centers.sameRuntimeOwnerAs(view.target))
    assert(neighborhoods.ambient.sameRuntimeOwnerAs(bridge.space))
    assertEquals(
      view.target.indices
        .map(neighborhoods.neighborhood)
        .map(_.indicesInDomainOrder.map(field.apply).toVector)
        .toVector,
      Vector(
        Vector(11.0),
        Vector(12.0),
        Vector(13.0),
        Vector(21.0),
        Vector(22.0),
        Vector(23.0)
      )
    )

  test("flip and permutation produce locus bijections"):
    val grid = persistentGrid2("bijection", Vector(3, 4))
    val bridge = register(grid, "bijection voxels").value
    val flip =
      right(bridge.exactView(imageRight(LatticeMap.flip[D2](grid.shape, 0))))
    val permutation =
      right(
        bridge.exactView(
          imageRight(LatticeMap.permute[D2](grid.shape, Vector(1, 0)))
        )
      )
    val region =
      right(Region.fromOrdinals(bridge.space, Vector(0, 3, 6, 11)))

    assert(flip.bijection.nonEmpty)
    assert(permutation.bijection.nonEmpty)
    assert(GridDomainLaws.bijectiveRegionPreservesCardinality(region, flip))
    assert(
      GridDomainLaws.bijectiveRegionPreservesCardinality(
        region,
        permutation
      )
    )

  private def persistentGrid2(
      suffix: String,
      shape: Vector[Int]
  ): Grid[? <: Frame[D2], D2] =
    val frame = persistentFrame[D2](suffix)
    right(
      Grid.createPersistent(gridId(s"grid-$suffix"), frame)(
        shape,
        Affine.identity[D2]
      )
    )

  private def persistentGrid3(
      suffix: String,
      shape: Vector[Int]
  ): Grid[? <: Frame[D3], D3] =
    val frame = persistentFrame[D3](suffix)
    right(
      Grid.createPersistent(gridId(s"grid-$suffix"), frame)(
        shape,
        Affine.identity[D3]
      )
    )

  private def persistentFrame[D <: Dim](
      suffix: String
  )(using Dimension[D]): Frame[D] =
    right(
      Frame.persistentNamed[D](
        frameId(s"frame-$suffix"),
        s"frame $suffix",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )

  private def register[F <: Frame[D], D <: Dim](
      grid: Grid[F, D],
      name: String
  ): GridDomainResolution[F, D] =
    right(GridDomain.register(grid, name, DomainRegistry.empty))

  private def index3(
      first: Int,
      second: Int,
      third: Int
  ): LatticeIndex[D3] =
    right(LatticeIndex.of[D3](first, second, third))

  private def coordinatesOfOrdinal(
      shape: Vector[Int],
      ordinal: Int
  ): Vector[Int] =
    val coordinates = Array.ofDim[Int](shape.size)
    var remaining = ordinal
    var axis = shape.size - 1
    while axis >= 0 do
      coordinates(axis) = remaining % shape(axis)
      remaining /= shape(axis)
      axis -= 1
    coordinates.toVector

  private def frameId(value: String): FrameId =
    right(FrameId.parse(value))

  private def gridId(value: String): GridId =
    right(GridId.parse(value))

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(s"expected Right, found Left($error)")

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    right(value)
