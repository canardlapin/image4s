package image4s.intaglio

import _root_.intaglio.RasterDimensions
import _root_.intaglio.RasterImage
import _root_.intaglio.RegularGridAxis
import _root_.intaglio.ScalarColorizer
import _root_.intaglio.ScalarField2D
import image4s.Categorical
import image4s.Continuous
import image4s.LinearInterpolable
import image4s.MaskImage
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.D2
import image4s.geometry.D3
import ravel.Rank

/** Lowers D2 continuous images to Intaglio display intermediates.
  *
  * This bridge owns no scientific image operation. Field lowering is available only for
  * axis-aligned affine grids. Raster lowering always retains raster pixels in source-index
  * coordinates; callers that need a rotated or sheared display either resample explicitly with
  * reframe4s or place the resulting raster in an Intaglio scene with the appropriate transform.
  */
object DisplayBridge:
  /** Lower an axis-aligned D2 scalar image into an Intaglio regular field.
    *
    * Reflected affine axes are represented by reversing display values so both Intaglio axes remain
    * increasing. Sheared or rotated grids are rejected rather than being silently reinterpreted as
    * a regular field.
    */
  def toIntaglioField[
      S <: SampleSpace[?, D2],
      A
  ](
      image: Sampled[S, A, Continuous, Rank[2]]
  )(using
      values: LinearInterpolable[A]
  ): Either[
    DisplayBridgeError,
    ScalarField2D
  ] =
    fieldGeometry(image.grid.indexToFrame.rowMajor, image.grid.indexToFrame.tolerance)
      .flatMap { geometry =>
        for
          xAxis <- RegularGridAxis
            .cellCentered(
              geometry.xLower(image.grid.shape(0)),
              geometry.xUpper(image.grid.shape(0)),
              image.grid.shape(0)
            )
            .left
            .map(error => DisplayBridgeError.IntaglioFailure(error.message))
          yAxis <- RegularGridAxis
            .cellCentered(
              geometry.yLower(image.grid.shape(1)),
              geometry.yUpper(image.grid.shape(1)),
              image.grid.shape(1)
            )
            .left
            .map(error => DisplayBridgeError.IntaglioFailure(error.message))
          field <- ScalarField2D(
            xAxis,
            yAxis,
            fieldValues(image, geometry)
          ).left.map(error => DisplayBridgeError.IntaglioFailure(error.message))
        yield field
      }

  /** Render a D2 scalar image into a visual-top-row raster.
    *
    * The source is not copied or reoriented before packing. The one primitive `Array[Int]` is
    * allocated by Intaglio's `RasterImage.tabulate`; source lookup, windowing, palette lookup, and
    * orientation all occur in its pixel loop.
    */
  def renderRaster[
      S <: SampleSpace[?, D2],
      A
  ](
      image: Sampled[S, A, Continuous, Rank[2]],
      plan: DisplayPlan
  )(using values: LinearInterpolable[A]): RasterImage =
    val sourceWidth = image.grid.shape(0)
    val sourceHeight = image.grid.shape(1)
    val orientation = plan.orientation
    val dimensions =
      RasterDimensions.unsafe(
        orientation.outputWidth(sourceWidth, sourceHeight),
        orientation.outputHeight(sourceWidth, sourceHeight)
      )
    val colorizer = ScalarColorizer(plan.window, plan.palette)

    RasterImage.tabulate(dimensions) { (x, y) =>
      val sourceX =
        sourceIndexX(x, y, sourceWidth, sourceHeight, orientation)
      val sourceY =
        sourceIndexY(x, y, sourceWidth, sourceHeight, orientation)
      colorizer.color(values.toDouble(image.data(sourceX, sourceY)))
    }

  /** Render a scalar D2 raster with a Boolean mask composited in the same display coordinate
    * mapping as the base raster.
    */
  def renderRasterWithMask[
      S <: SampleSpace[?, D2],
      A,
      MS <: SampleSpace[?, D2]
  ](
      image: Sampled[S, A, Continuous, Rank[2]],
      mask: MaskImage[MS, Rank[2]],
      plan: DisplayPlan,
      overlay: MaskOverlay
  )(using values: LinearInterpolable[A]): Either[DisplayBridgeError, RasterImage] =
    if image.logicalShape != mask.logicalShape then
      Left(
        DisplayBridgeError.IncompatibleOverlay(
          image.logicalShape,
          mask.logicalShape
        )
      )
    else
      val sourceWidth = image.grid.shape(0)
      val sourceHeight = image.grid.shape(1)
      val orientation = plan.orientation
      val dimensions =
        RasterDimensions.unsafe(
          orientation.outputWidth(sourceWidth, sourceHeight),
          orientation.outputHeight(sourceWidth, sourceHeight)
        )
      val colorizer = ScalarColorizer(plan.window, plan.palette)
      Right(
        RasterImage.tabulate(dimensions) { (x, y) =>
          val sourceX =
            sourceIndexX(x, y, sourceWidth, sourceHeight, orientation)
          val sourceY =
            sourceIndexY(x, y, sourceWidth, sourceHeight, orientation)
          val base = colorizer.color(values.toDouble(image.data(sourceX, sourceY)))
          if mask.data(sourceX, sourceY) then
            overlay.blend.composite(base, overlay.foreground, overlay.opacity)
          else base
        }
      )

  /** Render integer labels with a deterministic code-to-color palette. */
  def renderLabels[
      S <: SampleSpace[?, D2]
  ](
      labels: Sampled[S, Int, Categorical, Rank[2]],
      palette: LabelPalette = LabelPalette(),
      orientation: DisplayOrientation = DisplayOrientation.Identity
  ): RasterImage =
    val sourceWidth = labels.grid.shape(0)
    val sourceHeight = labels.grid.shape(1)
    val dimensions =
      RasterDimensions.unsafe(
        orientation.outputWidth(sourceWidth, sourceHeight),
        orientation.outputHeight(sourceWidth, sourceHeight)
      )
    RasterImage.tabulate(dimensions) { (x, y) =>
      val sourceX =
        sourceIndexX(x, y, sourceWidth, sourceHeight, orientation)
      val sourceY =
        sourceIndexY(x, y, sourceWidth, sourceHeight, orientation)
      palette.color(labels.data(sourceX, sourceY))
    }

  /** Lower one orthogonal D3 slice directly to an Intaglio raster.
    *
    * Slice axes are source-grid axes. This method owns only extraction and pixel packing; it does
    * not resample oblique slices or interpret affine geometry as a display transform.
    */
  def renderSliceRaster[
      S <: SampleSpace[?, D3],
      A
  ](
      image: Sampled[S, A, Continuous, Rank[3]],
      axis: SliceAxis,
      index: Int,
      plan: DisplayPlan
  )(using values: LinearInterpolable[A]): Either[DisplayBridgeError, RasterImage] =
    val fixedAxis =
      axis match
        case SliceAxis.X => 0
        case SliceAxis.Y => 1
        case SliceAxis.Z => 2
    val extent = image.grid.shape(fixedAxis)
    if index < 0 || index >= extent then
      Left(DisplayBridgeError.InvalidSliceIndex(axis, index, extent))
    else
      val sourceAxes = (0 until 3).filter(_ != fixedAxis).toVector
      val sourceWidth = image.grid.shape(sourceAxes(0))
      val sourceHeight = image.grid.shape(sourceAxes(1))
      val orientation = plan.orientation
      val dimensions =
        RasterDimensions.unsafe(
          orientation.outputWidth(sourceWidth, sourceHeight),
          orientation.outputHeight(sourceWidth, sourceHeight)
        )
      val colorizer = ScalarColorizer(plan.window, plan.palette)
      Right(
        RasterImage.tabulate(dimensions) { (x, y) =>
          val sourceX =
            sourceIndexX(x, y, sourceWidth, sourceHeight, orientation)
          val sourceY =
            sourceIndexY(x, y, sourceWidth, sourceHeight, orientation)
          val scalar =
            fixedAxis match
              case 0 => values.toDouble(image.data(index, sourceX, sourceY))
              case 1 => values.toDouble(image.data(sourceX, index, sourceY))
              case _ => values.toDouble(image.data(sourceX, sourceY, index))
          colorizer.color(scalar)
        }
      )

  private final case class FieldGeometry(
      originX: Double,
      spacingX: Double,
      originY: Double,
      spacingY: Double
  ):
    private def lower(origin: Double, spacing: Double, count: Int): Double =
      math.min(origin, origin + spacing * (count - 1).toDouble) -
        math.abs(spacing) / 2.0

    private def upper(origin: Double, spacing: Double, count: Int): Double =
      math.max(origin, origin + spacing * (count - 1).toDouble) +
        math.abs(spacing) / 2.0

    def xLower(count: Int): Double =
      lower(originX, spacingX, count)

    def xUpper(count: Int): Double =
      upper(originX, spacingX, count)

    def yLower(count: Int): Double =
      lower(originY, spacingY, count)

    def yUpper(count: Int): Double =
      upper(originY, spacingY, count)

  private def fieldGeometry(
      affine: Vector[Double],
      tolerance: Double
  ): Either[DisplayBridgeError, FieldGeometry] =
    val xSpacing = affine(0)
    val ySpacing = affine(4)
    if math.abs(affine(1)) > tolerance || math.abs(affine(3)) > tolerance then
      Left(DisplayBridgeError.NonAxisAlignedField(affine))
    else if !xSpacing.isFinite || xSpacing == 0.0 then
      Left(DisplayBridgeError.DegenerateFieldAxis(0, xSpacing))
    else if !ySpacing.isFinite || ySpacing == 0.0 then
      Left(DisplayBridgeError.DegenerateFieldAxis(1, ySpacing))
    else
      Right(
        FieldGeometry(
          originX = affine(2),
          spacingX = xSpacing,
          originY = affine(5),
          spacingY = ySpacing
        )
      )

  private def fieldValues[
      S <: SampleSpace[?, D2],
      A
  ](
      image: Sampled[S, A, Continuous, Rank[2]],
      geometry: FieldGeometry
  )(using values: LinearInterpolable[A]): Array[Double] =
    val width = image.grid.shape(0)
    val height = image.grid.shape(1)
    val result = new Array[Double](width * height)
    var y = 0
    while y < height do
      val sourceY = if geometry.spacingY > 0.0 then y else height - 1 - y
      var x = 0
      val offset = y * width
      while x < width do
        val sourceX = if geometry.spacingX > 0.0 then x else width - 1 - x
        result(offset + x) = values.toDouble(image.data(sourceX, sourceY))
        x += 1
      y += 1
    result

  private def sourceIndexX(
      x: Int,
      y: Int,
      sourceWidth: Int,
      sourceHeight: Int,
      orientation: DisplayOrientation
  ): Int =
    val outputWidth = orientation.outputWidth(sourceWidth, sourceHeight)
    val orientedX = if orientation.flipX then outputWidth - 1 - x else x
    val outputHeight = orientation.outputHeight(sourceWidth, sourceHeight)
    val orientedY = if orientation.flipY then outputHeight - 1 - y else y
    if orientation.transpose then orientedY else orientedX

  private def sourceIndexY(
      x: Int,
      y: Int,
      sourceWidth: Int,
      sourceHeight: Int,
      orientation: DisplayOrientation
  ): Int =
    val outputWidth = orientation.outputWidth(sourceWidth, sourceHeight)
    val orientedX = if orientation.flipX then outputWidth - 1 - x else x
    val outputHeight = orientation.outputHeight(sourceWidth, sourceHeight)
    val orientedY = if orientation.flipY then outputHeight - 1 - y else y
    if orientation.transpose then orientedX else orientedY
