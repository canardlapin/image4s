package image4s.locus

import image4s.Axis
import image4s.AxisKind
import image4s.AxisUnit
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import locus4s.DomainRegistry
import locus4s.Selection
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

final class SelectedSampledSuite extends FunSuite:
  test("selected scalar data retain exact selection order and Ravel storage"):
    val grid = persistentGrid("selected-scalar", Vector(2, 3, 2))
    val domain = register(grid, "scalar voxels")
    val selection =
      right(Selection.fromOrdinals(domain.space, Vector(7, 1, 10)))
    val data = NDArray.fromSeq(Shape(3), Vector(70.0, 10.0, 100.0))
    val selected =
      right(
        SelectedSampled.continuous(
          domain,
          selection,
          NonSpatialAxes.empty,
          data,
          ImageMetadata("selected")
        )
      )

    assert(selected.data eq data)
    assert(selected.selection eq selection)
    assertEquals(selected.logicalShape, Vector(3))
    assertEquals(selected.field.toVector, Vector(70.0, 10.0, 100.0))
    assertEquals(
      right(selected.valueAt(selected.selection.positions.indexOption(1).get)),
      10.0
    )

  test("gathered series use position-time order with contiguous voxel rows"):
    val grid = persistentGrid("selected-series", Vector(2, 3, 2))
    val domain = register(grid, "series voxels")
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          3,
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
          NDArray.tabulate[Double](2, 3, 2, 3): (x, y, z, t) =>
            1000.0 * x + 100.0 * y + 10.0 * z + t
        )
      )
    val selection =
      right(Selection.fromOrdinals(domain.space, Vector(7, 1, 10)))
    val selected =
      right(
        SelectedSampled.gatherSingleAxis(
          domain,
          image,
          selection
        )
      )

    assertEquals(selected.data.shape, Shape(3, 3))
    assertEquals(
      selected.data.iterator.toVector,
      Vector(
        1010.0, 1011.0, 1012.0, 10.0, 11.0, 12.0, 1200.0, 1201.0, 1202.0
      )
    )
    val firstPosition = selected.selection.positions.indexOption(0).get
    val firstSeries = selected.seriesAt(firstPosition)
    assert(firstSeries.isContiguous)
    assertEquals(firstSeries.iterator.toVector, Vector(1010.0, 1011.0, 1012.0))
    assertEquals(
      right(selected.fieldAt(Vector(2))).toVector,
      Vector(1012.0, 12.0, 1202.0)
    )
    val scattered = right(selected.scatter(-1.0))
    assertEquals(scattered.data.shape, Shape(2, 3, 2, 3))
    assertEquals(scattered.data(1, 0, 1, 2), 1012.0)
    assertEquals(scattered.data(0, 0, 1, 2), 12.0)
    assertEquals(scattered.data(1, 2, 0, 2), 1202.0)
    assertEquals(scattered.data(0, 0, 0, 2), -1.0)

    val retainedSelection =
      right(Selection.fromOrdinals(domain.space, Vector(1, 10)))
    val retainedData =
      NDArray.fromSeq(
        Shape(2, 3),
        Vector(10.0, 11.0, 12.0, 1200.0, 1201.0, 1202.0)
      )
    val retained =
      right(selected.withSelectionData(retainedSelection, retainedData))
    assert(retained.selection eq retainedSelection)
    assert(retained.data eq retainedData)
    assertEquals(retained.metadata, selected.metadata)

  test("generic gather preserves multiple trailing axes in canonical order"):
    val grid = persistentGrid("selected-multi-axis", Vector(2, 1, 2))
    val domain = register(grid, "multi-axis voxels")
    val trial = imageRight(Axis.create("trial", 2, AxisKind.Other))
    val feature = imageRight(Axis.create("feature", 3, AxisKind.Channel))
    val axes = imageRight(NonSpatialAxes.from(Vector(trial, feature)))
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.build[Double, ravel.AnyRank](
            right(Shape.from(Vector(2, 1, 2, 2, 3)))
          ): output =>
            var index = 0
            while index < 24 do
              output.writeLinear(index, index.toDouble)
              index += 1
        )
      )
    val selection =
      right(Selection.fromOrdinals(domain.space, Vector(2, 0)))
    val selected = right(SelectedSampled.gather(domain, image, selection))

    assertEquals(selected.logicalShape, Vector(2, 2, 3))
    assertEquals(
      selected.data.iterator.toVector,
      Vector(12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
    )

  test("construction and gather fail closed on shape and domain owner"):
    val grid = persistentGrid("selected-errors", Vector(2, 3, 2))
    val first = register(grid, "first owner")
    val second = register(grid, "second owner")
    val native =
      right(Selection.fromOrdinals(first.space, Vector(0, 3)))
    val foreign =
      right(Selection.fromOrdinals(second.space, Vector(0, 3)))
    val volume =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 3, 2): (x, y, z) =>
            100.0 * x + 10.0 * y + z
        )
      )

    assertEquals(
      SelectedSampled.continuous(
        first,
        native,
        NonSpatialAxes.empty,
        NDArray.fromSeq(Shape(1), Vector(1.0))
      ),
      Left(
        SelectedSampledError.DataShapeMismatch(
          Vector(2),
          Vector(1)
        )
      )
    )
    assert(
      SelectedSampled
        .gatherSpatial(first, volume, foreign)
        .left
        .toOption
        .exists:
          case SelectedSampledError.SelectionSpace(_) => true
          case _ => false
    )

  private def persistentGrid(
      suffix: String,
      shape: Vector[Int]
  ): Grid[? <: Frame[D3], D3] =
    val frame =
      right(
        Frame.persistentNamed[D3](
          right(FrameId.parse(s"frame-$suffix")),
          s"frame $suffix",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    right(
      Grid.createPersistent(
        right(GridId.parse(s"grid-$suffix")),
        frame
      )(shape, Affine.identity[D3])
    )

  private def register[F <: Frame[D3]](
      grid: Grid[F, D3],
      name: String
  ): GridDomain[F, D3, ?] =
    right(GridDomain.register(grid, name, DomainRegistry.empty)).value

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(s"expected Right, found Left($error)")

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    right(value)
