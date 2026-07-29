package image4s.locus

import locus4s.DomainFreshError
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainResolution
import locus4s.DomainRestoreError
import locus4s.FiniteSpace
import locus4s.Point as DomainPoint
import locus4s.PointError
import locus4s.SpaceMismatch
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.GridRecord
import image4s.geometry.Index

/** The sole supported linearization for a grid-backed finite domain.
  *
  * The last spatial axis varies fastest.
  */
enum GridDomainLayout:
  case RowMajorLastAxisFastest

/** Serializable evidence binding one persistent grid to one persistent domain. */
final case class GridDomainRecord(
    grid: GridRecord,
    domain: DomainRecord,
    layout: GridDomainLayout
)

sealed trait GridDomainError:
  def message: String

object GridDomainError:
  final case class VoxelCountOverflow(
      shape: Vector[Int],
      maximum: Int
  ) extends GridDomainError:
    val message: String =
      s"grid shape ${shape.mkString("[", ", ", "]")} exceeds the maximum " +
        s"finite-domain size $maximum"

  final case class DomainSizeMismatch(expected: Int, actual: Int)
      extends GridDomainError:
    val message: String =
      s"grid requires a finite domain of size $expected, found $actual"

  final case class SpatialIndexOutOfBounds(
      axis: Int,
      coordinate: Int,
      extent: Int
  ) extends GridDomainError:
    val message: String =
      s"grid index axis $axis is outside [0, $extent): $coordinate"

  final case class OrdinalOutOfBounds(ordinal: Int, size: Int)
      extends GridDomainError:
    val message: String =
      s"grid-domain ordinal is outside [0, $size): $ordinal"

  final case class GeometryFailure(error: GeometryError)
      extends GridDomainError:
    val message: String = error.message

  final case class DomainFreshFailure(error: DomainFreshError)
      extends GridDomainError:
    val message: String = error.message

  final case class DomainRestoreFailure(error: DomainRestoreError)
      extends GridDomainError:
    val message: String = error.message

  final case class DomainPointFailure(error: PointError)
      extends GridDomainError:
    val message: String = error.message

  final case class GridRecordMismatch(
      expected: GridRecord,
      actual: GridRecord
  ) extends GridDomainError:
    val message: String =
      s"bridge record names grid ${expected.id.value}, but the live grid is " +
        actual.id.value

  final case class LayoutMismatch(
      expected: GridDomainLayout,
      actual: GridDomainLayout
  ) extends GridDomainError:
    val message: String =
      s"bridge requires layout $expected, found $actual"

  final case class GridRuntimeOwnerMismatch(id: GridId)
      extends GridDomainError:
    val message: String =
      s"grid ${id.value} is represented by a different live runtime owner"

  final case class DomainRuntimeOwnerMismatch(error: SpaceMismatch)
      extends GridDomainError:
    val message: String = error.message

/** A checked live attachment of one spatial grid to one finite domain.
  *
  * `grid` and `space` remain the sole owners of their respective identities.
  * This bridge owns no image storage and introduces no parallel grid or domain
  * representation.
  */
final class GridDomain[
    F <: Frame[D],
    D <: Dim,
    S
] private (
    val grid: Grid[F, D],
    val space: FiniteSpace[S],
    val voxelCount: Int
):
  val layout: GridDomainLayout =
    GridDomainLayout.RowMajorLastAxisFastest

  def record: GridDomainRecord =
    GridDomainRecord(grid.record, space.record, layout)

  /** Convert a checked grid index to its last-axis-fastest row-major ordinal. */
  def ordinalOf(index: Index[D]): Either[GridDomainError, Int] =
    grid.shape.indices.collectFirst {
      case axis
          if index.values(axis) < 0 ||
            index.values(axis) >= grid.shape(axis) =>
        GridDomainError.SpatialIndexOutOfBounds(
          axis,
          index.values(axis),
          grid.shape(axis)
        )
    } match
      case Some(error) =>
        Left(error)
      case None =>
        var ordinal = 0L
        var axis = 0
        while axis < grid.shape.length do
          ordinal =
            ordinal * grid.shape(axis).toLong + index.values(axis).toLong
          axis += 1
        Right(ordinal.toInt)

  /** Convert a row-major ordinal back to its checked spatial index. */
  def indexOfOrdinal(
      ordinal: Int
  )(using Dimension[D]): Either[GridDomainError, Index[D]] =
    if ordinal < 0 || ordinal >= voxelCount then
      Left(GridDomainError.OrdinalOutOfBounds(ordinal, voxelCount))
    else
      val coordinates = Array.ofDim[Int](grid.shape.length)
      var remaining = ordinal
      var axis = grid.shape.length - 1
      while axis >= 0 do
        val extent = grid.shape(axis)
        coordinates(axis) = remaining % extent
        remaining = remaining / extent
        axis -= 1
      Index
        .fromVector[D](coordinates.toVector)
        .left
        .map(GridDomainError.GeometryFailure.apply)

  /** Resolve a grid index directly to its live finite-domain point. */
  def pointAt(index: Index[D]): Either[GridDomainError, DomainPoint[S]] =
    ordinalOf(index).flatMap: ordinal =>
      space
        .point(ordinal)
        .left
        .map(GridDomainError.DomainPointFailure.apply)

  /** Recover the spatial index of a point owned by this exact live domain. */
  def indexOf(
      point: DomainPoint[S]
  )(using Dimension[D]): Either[GridDomainError, Index[D]] =
    if space.contains(point) then indexOfOrdinal(point.value)
    else
      Left(
        GridDomainError.DomainRuntimeOwnerMismatch(
          SpaceMismatch(
            space.record,
            point.domain,
            space.record == point.domain
          )
        )
      )

  /** Require the exact live grid owner, not merely equal serialized metadata. */
  def validateGrid[G <: Frame[D]](
      candidate: Grid[G, D]
  ): Either[GridDomainError, Unit] =
    Grid.align(grid, candidate) match
      case Left(error) =>
        Left(GridDomainError.GeometryFailure(error))
      case Right(alignment) if alignment.sameRuntimeOwner =>
        Right(())
      case Right(_) =>
        Left(GridDomainError.GridRuntimeOwnerMismatch(grid.id))

  /** Require the exact live domain owner, not merely equal persistent metadata. */
  def validateSpace[T](
      candidate: FiniteSpace[T]
  ): Either[GridDomainError, Unit] =
    if space.sameRuntimeOwnerAs(candidate) then Right(())
    else
      Left(
        GridDomainError.DomainRuntimeOwnerMismatch(
          SpaceMismatch(
            space.record,
            candidate.record,
            space.record == candidate.record
          )
        )
      )

/** Fresh or restored bridge with an existential, registry-created domain owner. */
sealed trait GridDomainResolution[
    F <: Frame[D],
    D <: Dim
]:
  type S
  val registry: DomainRegistry
  val value: GridDomain[F, D, S]

object GridDomain:
  val Layout: GridDomainLayout =
    GridDomainLayout.RowMajorLastAxisFastest

  def attach[
      F <: Frame[D],
      D <: Dim,
      S
  ](
      grid: Grid[F, D],
      space: FiniteSpace[S]
  ): Either[GridDomainError, GridDomain[F, D, S]] =
    voxelCount(grid.shape).flatMap: count =>
      if space.size != count then
        Left(GridDomainError.DomainSizeMismatch(count, space.size))
      else
        Right(new GridDomain(grid, space, count))

  def fresh[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      domainName: String,
      registry: DomainRegistry
  ): Either[GridDomainError, GridDomainResolution[F, D]] =
    voxelCount(grid.shape).flatMap: count =>
      registry
        .fresh(domainName, count)
        .left
        .map(GridDomainError.DomainFreshFailure.apply)
        .flatMap(resolutionFrom(grid, _))

  def restore[
      F <: Frame[D],
      D <: Dim
  ](
      record: GridDomainRecord,
      grid: Grid[F, D],
      registry: DomainRegistry
  ): Either[GridDomainError, GridDomainResolution[F, D]] =
    if record.layout != Layout then
      Left(GridDomainError.LayoutMismatch(Layout, record.layout))
    else if record.grid != grid.record then
      Left(GridDomainError.GridRecordMismatch(record.grid, grid.record))
    else
      voxelCount(grid.shape).flatMap: count =>
        if record.domain.size != count then
          Left(
            GridDomainError.DomainSizeMismatch(count, record.domain.size)
          )
        else
          registry
            .restore(record.domain)
            .left
            .map(GridDomainError.DomainRestoreFailure.apply)
            .flatMap(resolutionFrom(grid, _))

  private final class Resolved[
      F <: Frame[D],
      D <: Dim,
      A
  ](
      val registry: DomainRegistry,
      val value: GridDomain[F, D, A]
  ) extends GridDomainResolution[F, D]:
    type S = A

  private def resolutionFrom[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      resolution: DomainResolution
  ): Either[GridDomainError, GridDomainResolution[F, D]] =
    attach(grid, resolution.space).map: bridge =>
      new Resolved(resolution.registry, bridge)

  private def voxelCount(
      shape: Vector[Int]
  ): Either[GridDomainError, Int] =
    var total = 1L
    var axis = 0
    var overflow = false
    while axis < shape.length && !overflow do
      val extent = shape(axis).toLong
      overflow = total > Int.MaxValue.toLong / extent
      if !overflow then total *= extent
      axis += 1

    if overflow then
      Left(GridDomainError.VoxelCountOverflow(shape, Int.MaxValue))
    else Right(total.toInt)
