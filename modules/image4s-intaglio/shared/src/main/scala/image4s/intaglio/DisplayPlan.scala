package image4s.intaglio

import _root_.intaglio.ColorRamp
import _root_.intaglio.DisplayBlendMode
import _root_.intaglio.DisplayOpacity
import _root_.intaglio.DisplayWindow
import _root_.intaglio.RasterInterpolation
import _root_.intaglio.Rgba32

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

/** Deterministic color assignment for integer labels.
  *
  * Code zero is transparent by default; every nonzero code maps to the same
  * opaque color independently of traversal order or process lifetime.
  */
final case class LabelPalette(
    background: Rgba32 = Rgba32.unsafe(0, 0, 0, 0)
):
  def color(code: Int): Rgba32 =
    if code == 0 then background
    else
      val mixed = mix(code)
      Rgba32.unsafe(
        48 + ((mixed >>> 16) & 0x9f),
        48 + ((mixed >>> 8) & 0x9f),
        48 + (mixed & 0x9f),
        255
      )

  private def mix(value: Int): Int =
    var state = value
    state ^= state >>> 16
    state *= 0x7feb352d
    state ^= state >>> 15
    state *= 0x846ca68b
    state ^ (state >>> 16)

/** Deterministic source-over appearance for a Boolean mask raster overlay. */
final case class MaskOverlay(
    foreground: Rgba32,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blend: DisplayBlendMode = DisplayBlendMode.Normal
)

/** Orthogonal D3 source axis fixed while lowering a two-dimensional slice. */
enum SliceAxis derives CanEqual:
  case X, Y, Z

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

  final case class IncompatibleOverlay(
      scalarShape: Vector[Int],
      maskShape: Vector[Int]
  ) extends DisplayBridgeError:
    val message: String =
      s"mask shape $maskShape does not match scalar raster shape $scalarShape"

  final case class InvalidSliceIndex(
      axis: SliceAxis,
      index: Int,
      extent: Int
  ) extends DisplayBridgeError:
    val message: String =
      s"slice index $index is outside $axis extent $extent"
