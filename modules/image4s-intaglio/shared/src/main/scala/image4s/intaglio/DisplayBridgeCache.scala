package image4s.intaglio

import _root_.intaglio.RasterImage
import image4s.Categorical
import image4s.Continuous
import image4s.LinearInterpolable
import image4s.MaskImage
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.D2
import image4s.geometry.D3
import ravel.Rank
import scala.collection.mutable

/** Memoizing wrapper over [[DisplayBridge]] raster lowering.
  *
  * Entries are keyed by source value identity (images are immutable, so
  * reference identity is a sound content key), the display plane, and the
  * structural fingerprints of the plan and overlay/palette appearance values.
  * Repeated renders with an identical key return the previously packed
  * [[RasterImage]] without touching source samples again.
  *
  * Eviction is least-recently-used with a fixed entry budget. The cache is
  * not synchronized; share one instance per rendering thread.
  */
final class DisplayBridgeCache(maxEntries: Int = 32):
  require(maxEntries > 0, s"cache needs a positive entry budget, got $maxEntries")

  private val entries = mutable.LinkedHashMap.empty[DisplayBridgeCache.Key, RasterImage]

  def entryCount: Int =
    entries.size

  def clear(): Unit =
    entries.clear()

  /** Cached [[DisplayBridge.renderRaster]]. */
  def renderRaster[
      S <: SampleSpace[?, D2],
      A
  ](
      image: Sampled[S, A, Continuous, Rank[2]],
      plan: DisplayPlan
  )(using values: LinearInterpolable[A]): RasterImage =
    cached(DisplayBridgeCache.Key(image, null, 1L, plan.fingerprint)) {
      DisplayBridge.renderRaster(image, plan)
    }

  /** Cached [[DisplayBridge.renderRasterWithMask]]. Errors are not cached. */
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
    val tag = DisplayPlan.mix(plan.fingerprint, overlay.fingerprint)
    val key = DisplayBridgeCache.Key(image, mask, 2L, tag)
    entries.get(key) match
      case Some(raster) =>
        touch(key, raster)
        Right(raster)
      case None =>
        DisplayBridge
          .renderRasterWithMask(image, mask, plan, overlay)
          .map { raster =>
            store(key, raster)
            raster
          }

  /** Cached [[DisplayBridge.renderLabels]]. */
  def renderLabels[
      S <: SampleSpace[?, D2]
  ](
      labels: Sampled[S, Int, Categorical, Rank[2]],
      palette: LabelPalette = LabelPalette(),
      orientation: DisplayOrientation = DisplayOrientation.Identity
  ): RasterImage =
    val tag =
      DisplayPlan.mix(palette.fingerprint, orientation.packedBits.toLong)
    cached(DisplayBridgeCache.Key(labels, null, 3L, tag)) {
      DisplayBridge.renderLabels(labels, palette, orientation)
    }

  /** Cached [[DisplayBridge.renderSliceRaster]]. Errors are not cached. */
  def renderSliceRaster[
      S <: SampleSpace[?, D3],
      A
  ](
      image: Sampled[S, A, Continuous, Rank[3]],
      axis: SliceAxis,
      index: Int,
      plan: DisplayPlan
  )(using values: LinearInterpolable[A]): Either[DisplayBridgeError, RasterImage] =
    val tag =
      DisplayPlan.mix(
        DisplayPlan.mix(plan.fingerprint, axis.ordinal.toLong),
        index.toLong
      )
    val key = DisplayBridgeCache.Key(image, null, 4L, tag)
    entries.get(key) match
      case Some(raster) =>
        touch(key, raster)
        Right(raster)
      case None =>
        DisplayBridge
          .renderSliceRaster(image, axis, index, plan)
          .map { raster =>
            store(key, raster)
            raster
          }

  private def cached(
      key: DisplayBridgeCache.Key
  )(render: => RasterImage): RasterImage =
    entries.get(key) match
      case Some(raster) =>
        touch(key, raster)
        raster
      case None =>
        val raster = render
        store(key, raster)
        raster

  private def touch(key: DisplayBridgeCache.Key, raster: RasterImage): Unit =
    entries.remove(key): Unit
    entries.update(key, raster)

  private def store(key: DisplayBridgeCache.Key, raster: RasterImage): Unit =
    entries.update(key, raster)
    while entries.size > maxEntries do
      entries.remove(entries.head._1): Unit

object DisplayBridgeCache:
  /** Reference-identity source key plus method tag and appearance fingerprint. */
  private final class Key(
      val primary: AnyRef,
      val secondary: AnyRef | Null,
      val method: Long,
      val appearance: Long
  ):
    override def hashCode: Int =
      var state = System.identityHashCode(primary)
      state = state * 31 + System.identityHashCode(secondary)
      state = state * 31 + method.hashCode
      state * 31 + appearance.hashCode

    override def equals(other: Any): Boolean =
      other match
        case that: Key =>
          (primary eq that.primary) &&
          (secondary eq that.secondary) &&
          method == that.method &&
          appearance == that.appearance
        case _ =>
          false
