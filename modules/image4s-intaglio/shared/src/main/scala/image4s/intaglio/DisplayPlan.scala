package image4s.intaglio

import _root_.intaglio.ColorRamp
import _root_.intaglio.DisplayWindow
import _root_.intaglio.RasterInterpolation

/** Pure appearance choices for lowering a scalar image to Intaglio.
  *
  * A display plan never changes sampled values or geometry. In particular, it
  * contains no resampling or filtering policy.
  */
final case class DisplayPlan(
    window: DisplayWindow,
    palette: ColorRamp = ColorRamp.Grayscale,
    orientation: DisplayOrientation = DisplayOrientation.Identity,
    interpolation: RasterInterpolation = RasterInterpolation.Nearest
)

/** Pixel-space orientation resolved during raster packing.
  *
  * Flips and transpose affect only display placement. They never create a
  * transformed `Sampled` value.
  */
final case class DisplayOrientation(
    transpose: Boolean = false,
    flipX: Boolean = false,
    flipY: Boolean = false
):
  def outputWidth(sourceWidth: Int, sourceHeight: Int): Int =
    if transpose then sourceHeight else sourceWidth

  def outputHeight(sourceWidth: Int, sourceHeight: Int): Int =
    if transpose then sourceWidth else sourceHeight

object DisplayOrientation:
  val Identity: DisplayOrientation =
    DisplayOrientation()

/** Errors raised while lowering image4s values into Intaglio display values. */
sealed trait DisplayBridgeError:
  def message: String

object DisplayBridgeError:
  final case class NonAxisAlignedField(
      affineRowMajor: Vector[Double]
  ) extends DisplayBridgeError:
    val message: String =
      "a scalar field requires an axis-aligned separable D2 affine; " +
        "resample with reframe4s or render a raster and place it with a scene transform"

  final case class DegenerateFieldAxis(
      axis: Int,
      spacing: Double
  ) extends DisplayBridgeError:
    val message: String =
      s"field axis $axis has zero or non-finite spacing $spacing"

  final case class IntaglioFailure(
      detail: String
  ) extends DisplayBridgeError:
    val message: String =
      detail
