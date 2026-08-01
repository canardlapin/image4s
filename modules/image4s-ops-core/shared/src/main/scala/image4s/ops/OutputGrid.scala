package image4s.ops

import image4s.LatticeMap
import image4s.geometry.Affine
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid

/** Exact output-grid laws for neighborhood filters.
  *
  * - `Same`: identical shape, grid identity, affine, and frame
  * - `Valid`: `n' = n - l - r`; new origin is old index `l`
  * - `Full`: `n' = n + l + r`; new origin is old index `-l`
  *
  * Geometry-preserving operations reuse the source grid. Geometry-changing
  * operations construct a fresh grid in the same frame.
  */
object OutputGrid:
  def shape[D <: Dim](
      sourceShape: Vector[Int],
      support: Support[D],
      extent: FilterExtent[?]
  )(using dimension: Dimension[D]): Either[OpError, Vector[Int]] =
    validateSourceShape(sourceShape).flatMap { shape =>
      val left = support.leftExtents
      val right = support.rightExtents
      extent match
        case FilterExtent.Same(_) =>
          Right(shape)
        case FilterExtent.Valid =>
          val next =
            Vector.tabulate(dimension.rank) { axis =>
              shape(axis) - left(axis) - right(axis)
            }
          next.zipWithIndex
            .collectFirst {
              case (extent, axis) if extent <= 0 =>
                OpError.InvalidExtent(
                  s"Valid extent is non-positive on axis $axis (got $extent)"
                )
            }
            .toLeft(next)
        case FilterExtent.Full(_) =>
          Right(
            Vector.tabulate(dimension.rank) { axis =>
              shape(axis) + left(axis) + right(axis)
            }
          )
    }

  def grid[F <: Frame[D], D <: Dim](
      source: Grid[F, D],
      support: Support[D],
      extent: FilterExtent[?]
  )(using dimension: Dimension[D]): Either[OpError, Grid[F, D]] =
    extent match
      case FilterExtent.Same(_) =>
        Right(source)
      case FilterExtent.Valid =>
        for
          targetShape <- shape(source.shape, support, extent)
          left = support.leftExtents
          origin = left
          lattice <- LatticeMap
            .crop(source.shape, origin, targetShape)
            .left
            .map(OpError.Image.apply)
          target <- lattice
            .targetGrid(source)
            .left
            .map(OpError.Image.apply)
        yield target
      case FilterExtent.Full(_) =>
        for
          targetShape <- shape(source.shape, support, extent)
          left = support.leftExtents
          delta = left.map(extent => -extent)
          shift <- indexTranslation(delta)
          targetAffine <- shift
            .andThen(source.indexToFrame)
            .left
            .map(OpError.Geometry.apply)
          target <- Grid
            .forFrame[D, F](source.frame)(targetShape, targetAffine)
            .left
            .map(OpError.Geometry.apply)
        yield target

  /** Homogeneous translation in index space: `old = new + delta`. */
  def indexTranslation[D <: Dim](
      delta: Vector[Int]
  )(using dimension: Dimension[D]): Either[OpError, Affine[D]] =
    if delta.length != dimension.rank then
      Left(
        OpError.InvalidExtent(
          s"translation rank ${delta.length} does not match spatial rank ${dimension.rank}"
        )
      )
    else
      val rank = dimension.rank
      val size = rank + 1
      val values =
        Vector.tabulate(size * size) { flat =>
          val row = flat / size
          val column = flat % size
          if row == rank && column == rank then 1.0
          else if row < rank && column == rank then delta(row).toDouble
          else if row == column then 1.0
          else 0.0
        }
      Affine
        .fromRowMajor[D](values)
        .left
        .map(OpError.Geometry.apply)

  private def validateSourceShape[D <: Dim](
      sourceShape: Vector[Int]
  )(using dimension: Dimension[D]): Either[OpError, Vector[Int]] =
    if sourceShape.length != dimension.rank then
      Left(
        OpError.InvalidExtent(
          s"source shape rank ${sourceShape.length} does not match spatial rank ${dimension.rank}"
        )
      )
    else
      sourceShape.zipWithIndex
        .collectFirst {
          case (extent, axis) if extent <= 0 =>
            OpError.InvalidExtent(
              s"source extent on axis $axis must be positive, got $extent"
            )
        }
        .toLeft(sourceShape)
