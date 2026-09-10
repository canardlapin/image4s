package image4s

import image4s.geometry.{Affine, CoordinateConvention, D2, D3, Frame, Grid, LengthUnit}

class SampleSpaceRefinementSuite extends munit.FunSuite:
  private def right[E, A](result: Either[E, A]): A =
    result.fold(error => fail(error.toString), identity)

  test("D3 refinement preserves the exact space, owner, physical declaration and sampled axes"):
    val frame = right(Frame.named[D3]("scanner", LengthUnit.Meter, CoordinateConvention.LPS))
    val grid = right(Grid.in(frame)(Vector(2, 3, 4), Affine.identity[D3]))
    val time = right(Axis.regular("time", AxisKind.Time, 5, 1.5, 0.25, AxisUnit.Seconds))
    val axes = right(NonSpatialAxes.from(Vector(time)))
    val erased: SomeSampleSpace = SampleSpace.create(grid, axes)
    val refined = right(erased.requireD3)
    assert(refined eq erased)
    assert(refined.grid eq grid)
    assert(refined.grid.frame eq frame)
    assert(refined.nonSpatialAxes eq axes)
    assertEquals(refined.grid.frame.unit, LengthUnit.Meter)
    assertEquals(refined.grid.frame.convention, CoordinateConvention.LPS)
    assertEquals(refined.grid.frame.persistentKey, None)
    assertEquals(erased.requireD2, Left(ImageError.SpatialDimensionMismatch(2, 3)))
    assert(right(refined.requireD3) eq refined)

  test("D2 refinement rejects D3 and does not merge equal-looking owners"):
    val first = right(Frame.named[D2]("slice", convention = CoordinateConvention.RAS))
    val second = right(Frame.named[D2]("slice", convention = CoordinateConvention.RAS))
    val a: SomeSampleSpace = SampleSpace.create(
      right(Grid.in(first)(Vector(2, 3), Affine.identity[D2])),
      NonSpatialAxes.empty
    )
    val b: SomeSampleSpace = SampleSpace.create(
      right(Grid.in(second)(Vector(2, 3), Affine.identity[D2])),
      NonSpatialAxes.empty
    )
    val ra = right(a.requireD2)
    val rb = right(b.requireD2)
    assert(ra eq a)
    assert(rb eq b)
    assert(!ra.grid.frame.sameRuntimeOwnerAs(rb.grid.frame))
    assertEquals(a.requireD3, Left(ImageError.SpatialDimensionMismatch(3, 2)))
