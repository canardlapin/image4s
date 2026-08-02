package image4s.intaglio

import _root_.intaglio.ColorRamp
import _root_.intaglio.DisplayBlendMode
import _root_.intaglio.DisplayOpacity
import _root_.intaglio.DisplayWindow
import _root_.intaglio.RasterInterpolation
import _root_.intaglio.Rgba32
import _root_.intaglio.toPackedInt

/** Pure appearance choices for lowering a scalar image to Intaglio.
  *
  * A display plan never changes sampled values or geometry. In particular, it
  * contains no resampling or filtering policy.
  *
  * The v0.1 display vocabulary is deliberately decomposed rather than folded
  * into one record: the D3 plane is an explicit `renderSliceRaster` argument,
  * the transfer function is the window plus palette pair, overlay and label
  * alpha ride on [[MaskOverlay]] and [[LabelPalette]], and multi-channel
  * display is out of scope. `interpolation` is a scene-scale placement hint
  * consumed by Intaglio backends; the bridge itself never resamples.
  */
final case class DisplayPlan(
    window: DisplayWindow,
    palette: ColorRamp = ColorRamp.Grayscale,
    orientation: DisplayOrientation = DisplayOrientation.Identity,
    interpolation: RasterInterpolation = RasterInterpolation.Nearest
):
  /** Deterministic content fingerprint over every appearance choice.
    *
    * Two plans share a fingerprint exactly when they render identically, so
    * the fingerprint (with the source identity) keys the bridge cache.
    */
  def fingerprint: Long =
    var state = DisplayPlan.mix(DisplayPlan.seed, java.lang.Double.doubleToRawLongBits(window.lower))
    state = DisplayPlan.mix(state, java.lang.Double.doubleToRawLongBits(window.upper))
    state = DisplayPlan.mix(state, palette.low.toPackedInt.toLong)
    state = DisplayPlan.mix(state, palette.high.toPackedInt.toLong)
    state = DisplayPlan.mix(state, orientation.packedBits.toLong)
    DisplayPlan.mix(state, interpolation.ordinal.toLong)

object DisplayPlan:
  private[intaglio] val seed: Long = 0xcbf29ce484222325L

  private[intaglio] def mix(state: Long, value: Long): Long =
    (state ^ value) * 0x100000001b3L

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

  private[intaglio] def packedBits: Int =
    (if transpose then 4 else 0) |
      (if flipX then 2 else 0) |
      (if flipY then 1 else 0)

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

  /** Deterministic content fingerprint for cache keying. */
  def fingerprint: Long =
    DisplayPlan.mix(DisplayPlan.seed, background.toPackedInt.toLong)

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
):
  /** Deterministic content fingerprint for cache keying. */
  def fingerprint: Long =
    var state = DisplayPlan.mix(DisplayPlan.seed, foreground.toPackedInt.toLong)
    state = DisplayPlan.mix(
      state,
      java.lang.Double.doubleToRawLongBits(opacity.toDouble)
    )
    DisplayPlan.mix(state, blend.ordinal.toLong)

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
