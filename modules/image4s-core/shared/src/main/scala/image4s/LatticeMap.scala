package image4s

import image4s.geometry.Affine
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.AnyRank
import ravel.NDArray
import ravel.Slice

/** Exact integer-affine map from a target spatial lattice into a source.
  *
  * Every target axis maps to exactly one source axis with a nonzero signed step. This restricted
  * representation is deliberate: every valid map can be represented by Ravel slicing and axis
  * permutation without copying values.
  */
final class LatticeMap[D <: Dim] private (
    val sourceShape: Vector[Int],
    val targetShape: Vector[Int],
    val origin: Vector[Int],
    val sourceAxisForTarget: Vector[Int],
    val steps: Vector[Int]
):
  def spatialRank: Int =
    sourceShape.size

  def isIdentity: Boolean =
    sourceShape == targetShape &&
      origin.forall(_ == 0) &&
      sourceAxisForTarget == sourceShape.indices.toVector &&
      steps.forall(_ == 1)

  /** Map one checked target coordinate to its exact source coordinate. */
  def sourceIndex(
      target: Vector[Int]
  ): Either[ImageError, Vector[Int]] =
    if target.size != spatialRank then
      Left(
        ImageError.LatticeTargetIndexRankMismatch(
          spatialRank,
          target.size
        )
      )
    else
      target.zip(targetShape).zipWithIndex.collectFirst {
        case ((index, extent), axis) if index < 0 || index >= extent =>
          ImageError.LatticeTargetIndexOutOfBounds(axis, index, extent)
      } match
        case Some(error) => Left(error)
        case None => Right(sourceIndexUnchecked(target))

  /** Compose this target-to-source map with a following map.
    *
    * If this map is `middle -> source` and `next` is `target -> middle`, the result is
    * `target -> source`.
    */
  def followedBy(
      next: LatticeMap[D]
  )(using Dimension[D]): Either[ImageError, LatticeMap[D]] =
    if targetShape != next.sourceShape then
      Left(
        ImageError.LatticeMapCompositionMismatch(
          targetShape,
          next.sourceShape
        )
      )
    else
      val combinedOrigin =
        sourceIndexUnchecked(next.origin)
      val combinedAxes =
        next.sourceAxisForTarget.map(sourceAxisForTarget)
      val combinedSteps =
        next.sourceAxisForTarget.zip(next.steps).map { case (middleAxis, nextStep) =>
          steps(middleAxis) * nextStep
        }
      LatticeMap.stridedPermutation(
        sourceShape,
        next.targetShape,
        combinedOrigin,
        combinedAxes,
        combinedSteps
      )

  /** Affine extension of this exact discrete map to real coordinates. */
  def toSourceAffine(using
      dimension: Dimension[D]
  ): Either[ImageError, Affine[D]] =
    val rank = dimension.rank
    val size = rank + 1
    val targetAxisForSource =
      Vector.tabulate(rank)(sourceAxisForTarget.indexOf)
    val values =
      Vector.tabulate(size * size) { flat =>
        val row = flat / size
        val column = flat % size
        if row == rank && column == rank then 1.0
        else if row < rank && column == rank then origin(row).toDouble
        else if row < rank && column < rank then
          val targetAxis = targetAxisForSource(row)
          if column == targetAxis then steps(targetAxis).toDouble else 0.0
        else 0.0
      }
    Affine
      .fromRowMajor[D](values)
      .left
      .map(ImageError.Geometry.apply)

  /** Construct the exact target geometry by composing index maps. */
  def targetGrid[F <: Frame[D]](
      source: Grid[F, D]
  )(using
      dimension: Dimension[D]
  ): Either[
    ImageError,
    Grid[F, D]
  ] =
    if source.shape != sourceShape then
      Left(
        ImageError.LatticeMapSourceShapeMismatch(
          source.shape,
          sourceShape
        )
      )
    else
      for
        discrete <- toSourceAffine
        targetAffine <- discrete
          .andThen(source.indexToFrame)
          .left
          .map(ImageError.Geometry.apply)
        target <- Grid
          .forFrame[D, F](source.frame)(
            targetShape,
            targetAffine
          )
          .left
          .map(ImageError.Geometry.apply)
      yield target

  private[image4s] def applyView[A, R <: AnyRank](
      source: NDArray[A, R]
  ): NDArray[A, R] =
    var sliced = source
    var sourceAxis = 0
    while sourceAxis < spatialRank do
      val targetAxis = sourceAxisForTarget.indexOf(sourceAxis)
      val start = origin(sourceAxis)
      val step = steps(targetAxis)
      val rawStop =
        start.toLong + step.toLong * targetShape(targetAxis).toLong
      val stop =
        if step < 0 && rawStop < -1L then -1
        else if step > 0 && rawStop > sourceShape(sourceAxis).toLong then sourceShape(sourceAxis)
        else rawStop.toInt
      sliced = sliced.slice(sourceAxis, Slice(start, stop, step))
      sourceAxis += 1
    val trailing = Vector.range(spatialRank, source.rank)
    sliced.permuteAxes((sourceAxisForTarget ++ trailing)*)

  private def sourceIndexUnchecked(target: Vector[Int]): Vector[Int] =
    val source = origin.toArray
    var targetAxis = 0
    while targetAxis < spatialRank do
      val sourceAxis = sourceAxisForTarget(targetAxis)
      source(sourceAxis) = origin(sourceAxis) + steps(targetAxis) * target(targetAxis)
      targetAxis += 1
    source.toVector

object LatticeMap:
  def identity[D <: Dim](
      shape: IterableOnce[Int]
  )(using dimension: Dimension[D]): Either[ImageError, LatticeMap[D]] =
    val copied = shape.iterator.toVector
    stridedPermutation(
      copied,
      copied,
      Vector.fill(dimension.rank)(0),
      Vector.range(0, dimension.rank),
      Vector.fill(dimension.rank)(1)
    )

  def crop[D <: Dim](
      sourceShape: IterableOnce[Int],
      origin: IterableOnce[Int],
      targetShape: IterableOnce[Int]
  )(using dimension: Dimension[D]): Either[ImageError, LatticeMap[D]] =
    val source = sourceShape.iterator.toVector
    val copiedOrigin = origin.iterator.toVector
    val target = targetShape.iterator.toVector
    if copiedOrigin.size != dimension.rank ||
      target.size != dimension.rank
    then
      Left(
        ImageError.SpatialViewRankMismatch(
          dimension.rank,
          copiedOrigin.size,
          target.size
        )
      )
    else
      target.zipWithIndex
        .collectFirst {
          case (extent, axis) if extent <= 0 =>
            ImageError.NonPositiveSpatialViewExtent(axis, extent)
        }
        .orElse(
          copiedOrigin.indices.collectFirst {
            case axis
                if source.lift(axis).isEmpty ||
                  copiedOrigin(axis) < 0 ||
                  copiedOrigin(axis).toLong + target(axis).toLong >
                  source(axis).toLong =>
              ImageError.SpatialViewOutOfBounds(
                axis,
                copiedOrigin(axis),
                target(axis),
                source.lift(axis).getOrElse(0)
              )
          }
        ) match
        case Some(error) => Left(error)
        case None =>
          stridedPermutation(
            source,
            target,
            copiedOrigin,
            Vector.range(0, dimension.rank),
            Vector.fill(dimension.rank)(1)
          )

  def flip[D <: Dim](
      sourceShape: IterableOnce[Int],
      axis: Int
  )(using dimension: Dimension[D]): Either[ImageError, LatticeMap[D]] =
    val source = sourceShape.iterator.toVector
    if axis < 0 || axis >= dimension.rank then
      Left(ImageError.InvalidSpatialAxis(axis, dimension.rank))
    else
      val origin =
        Vector.tabulate(dimension.rank) { current =>
          if current == axis then source.lift(current).getOrElse(0) - 1
          else 0
        }
      val steps =
        Vector.tabulate(dimension.rank) { current =>
          if current == axis then -1 else 1
        }
      stridedPermutation(
        source,
        source,
        origin,
        Vector.range(0, dimension.rank),
        steps
      )

  def permute[D <: Dim](
      sourceShape: IterableOnce[Int],
      sourceAxisForTarget: IterableOnce[Int]
  )(using dimension: Dimension[D]): Either[ImageError, LatticeMap[D]] =
    val source = sourceShape.iterator.toVector
    val order = sourceAxisForTarget.iterator.toVector
    if order.size == dimension.rank &&
      order.sorted != Vector.range(0, dimension.rank)
    then
      Left(
        ImageError.InvalidLatticeAxisPermutation(
          order,
          dimension.rank
        )
      )
    else
      val target =
        if source.size == dimension.rank &&
          order.size == dimension.rank
        then order.map(source)
        else Vector.empty
      stridedPermutation(
        source,
        target,
        Vector.fill(dimension.rank)(0),
        order,
        Vector.fill(dimension.rank)(1)
      )

  def stride[D <: Dim](
      sourceShape: IterableOnce[Int],
      steps: IterableOnce[Int]
  )(using dimension: Dimension[D]): Either[ImageError, LatticeMap[D]] =
    val source = sourceShape.iterator.toVector
    val copiedSteps = steps.iterator.toVector
    copiedSteps.zipWithIndex.collectFirst {
      case (step, axis) if step <= 0 =>
        ImageError.InvalidSpatialStride(axis, step)
    } match
      case Some(error) => Left(error)
      case None =>
        val target =
          if source.size == copiedSteps.size then
            source.zip(copiedSteps).map { case (extent, step) =>
              (extent - 1) / step + 1
            }
          else Vector.empty
        stridedPermutation(
          source,
          target,
          Vector.fill(dimension.rank)(0),
          Vector.range(0, dimension.rank),
          copiedSteps
        )

  def stridedPermutation[D <: Dim](
      sourceShape: IterableOnce[Int],
      targetShape: IterableOnce[Int],
      origin: IterableOnce[Int],
      sourceAxisForTarget: IterableOnce[Int],
      steps: IterableOnce[Int]
  )(using dimension: Dimension[D]): Either[ImageError, LatticeMap[D]] =
    val source = sourceShape.iterator.toVector
    val target = targetShape.iterator.toVector
    val copiedOrigin = origin.iterator.toVector
    val axes = sourceAxisForTarget.iterator.toVector
    val copiedSteps = steps.iterator.toVector
    val rank = dimension.rank
    if source.size != rank ||
      target.size != rank ||
      copiedOrigin.size != rank ||
      axes.size != rank ||
      copiedSteps.size != rank
    then
      Left(
        ImageError.LatticeMapRankMismatch(
          rank,
          source.size,
          target.size,
          copiedOrigin.size,
          axes.size,
          copiedSteps.size
        )
      )
    else
      source.zipWithIndex
        .collectFirst {
          case (extent, axis) if extent <= 0 =>
            ImageError.NonPositiveLatticeMapExtent("source", axis, extent)
        }
        .orElse(
          target.zipWithIndex.collectFirst {
            case (extent, axis) if extent <= 0 =>
              ImageError.NonPositiveLatticeMapExtent("target", axis, extent)
          }
        )
        .orElse(
          Option.when(axes.sorted != Vector.range(0, rank))(
            ImageError.InvalidLatticeAxisPermutation(axes, rank)
          )
        )
        .orElse(
          copiedSteps.zipWithIndex.collectFirst { case (0, targetAxis) =>
            ImageError.ZeroLatticeStep(targetAxis)
          }
        )
        .orElse(
          axes.indices.collectFirst {
            case targetAxis
                if !endpointsFit(
                  copiedOrigin(axes(targetAxis)),
                  copiedSteps(targetAxis),
                  target(targetAxis),
                  source(axes(targetAxis))
                ) =>
              val sourceAxis = axes(targetAxis)
              val first = copiedOrigin(sourceAxis).toLong
              val last =
                first +
                  copiedSteps(targetAxis).toLong *
                  (target(targetAxis).toLong - 1L)
              ImageError.LatticeMapOutOfBounds(
                sourceAxis,
                first,
                last,
                source(sourceAxis)
              )
          }
        ) match
        case Some(error) => Left(error)
        case None =>
          Right(
            new LatticeMap(
              source,
              target,
              copiedOrigin,
              axes,
              copiedSteps
            )
          )

  private def endpointsFit(
      first: Int,
      step: Int,
      extent: Int,
      sourceExtent: Int
  ): Boolean =
    val left = first.toLong
    val right =
      left + step.toLong * (extent.toLong - 1L)
    left >= 0L &&
    left < sourceExtent.toLong &&
    right >= 0L &&
    right < sourceExtent.toLong
