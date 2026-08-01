package image4s.locus

import image4s.ImageError
import image4s.LatticeMap
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.GridKey
import image4s.geometry.GridRecord
import image4s.geometry.LatticeIndex
import locus4s.Bijection
import locus4s.CertifiedMapError
import locus4s.DomainError
import locus4s.DomainFingerprint
import locus4s.DomainId
import locus4s.DomainKey
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainResolution
import locus4s.DomainRestoreError
import locus4s.FiniteDomain
import locus4s.FiniteSpace
import locus4s.Index as DomainIndex
import locus4s.IndexError
import locus4s.Injection
import locus4s.TotalMapError
import ravel.AnyRank

/** Versioned logical linearization of a grid-backed finite domain.
  *
  * This convention is independent of Ravel's physical layout. Versioning is
  * part of persistent identity, so a future convention cannot silently
  * reinterpret existing voxel ordinals.
  */
enum GridDomainLayout(val id: String):
  case RowMajorLastAxisFastestV1
      extends GridDomainLayout("row-major-last-axis-fastest/v1")
  case Unrecognized(value: String)
      extends GridDomainLayout(value)

/** Serializable evidence binding one persistent grid to its canonical domain. */
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

  final case class PersistentGridRequired(error: GeometryError)
      extends GridDomainError:
    val message: String =
      s"a canonical grid domain requires a persistent grid: ${error.message}"

  final case class GeometryFailure(error: GeometryError)
      extends GridDomainError:
    val message: String = error.message

  final case class ImageFailure(error: ImageError)
      extends GridDomainError:
    val message: String = error.message

  final case class DomainRecordFailure(error: DomainError)
      extends GridDomainError:
    val message: String = error.message

  final case class DomainRestoreFailure(error: DomainRestoreError)
      extends GridDomainError:
    val message: String = error.message

  final case class DomainIndexFailure(error: IndexError)
      extends GridDomainError:
    val message: String = error.message

  final case class LocusMapFailure(message: String)
      extends GridDomainError

  final case class GridRecordMismatch(
      expected: GridRecord,
      actual: GridRecord
  ) extends GridDomainError:
    val message: String =
      s"bridge record names grid ${expected.key.id.value}, but the live grid is " +
        actual.key.id.value

  final case class DomainKeyMismatch(
      expected: DomainKey,
      actual: DomainKey
  ) extends GridDomainError:
    val message: String =
      s"grid requires canonical voxel-domain key $expected, found $actual"

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

  final case class DomainRuntimeOwnerMismatch(
      expected: DomainRecord,
      actual: DomainRecord
  ) extends GridDomainError:
    val message: String =
      s"domain ${expected.id.value} is represented by a different live runtime owner"

  final case class ImageGridRuntimeOwnerMismatch(id: GridId)
      extends GridDomainError:
    val message: String =
      s"image grid ${id.value} is not the bridge's live grid owner"

  final case class SpatialFieldRequiresNoNonSpatialAxes(actual: Int)
      extends GridDomainError:
    val message: String =
      s"a spatial field requires no non-spatial axes, found $actual"

  case object SeriesFieldRequiresNonSpatialAxis extends GridDomainError:
    val message: String =
      "a voxel-series field requires at least one non-spatial axis"

/** A checked attachment of one persistent grid to its canonical finite domain.
  *
  * The bridge owns no image storage. Grid geometry, Ravel storage, and locus
  * domain identity remain owned by their respective libraries.
  */
final class GridDomain[
    F <: Frame[D],
    D <: Dim,
    S
] private (
    val grid: Grid[F, D],
    val space: FiniteSpace[S],
    val voxelCount: Int,
    private val persistentGridRecord: GridRecord
):
  val layout: GridDomainLayout =
    GridDomain.Layout

  def record: GridDomainRecord =
    GridDomainRecord(persistentGridRecord, space.record, layout)

  /** Convert a lattice coordinate to last-axis-fastest row-major ordinal. */
  def ordinalOf(index: LatticeIndex[D]): Either[GridDomainError, Int] =
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
        Right(ordinalOfCoordinatesUnchecked(index.values))

  /** Convert a row-major ordinal back to its checked spatial coordinate. */
  def indexOfOrdinal(
      ordinal: Int
  )(using Dimension[D]): Either[GridDomainError, LatticeIndex[D]] =
    if ordinal < 0 || ordinal >= voxelCount then
      Left(GridDomainError.OrdinalOutOfBounds(ordinal, voxelCount))
    else
      LatticeIndex
        .fromVector[D](coordinatesOfOrdinalUnchecked(ordinal))
        .left
        .map(GridDomainError.GeometryFailure.apply)

  /** Resolve a lattice coordinate to its typed locus index. */
  def domainIndexAt(
      index: LatticeIndex[D]
  ): Either[GridDomainError, DomainIndex[S]] =
    ordinalOf(index).flatMap: ordinal =>
      space
        .index(ordinal)
        .left
        .map(GridDomainError.DomainIndexFailure.apply)

  /** Compatibility spelling for [[domainIndexAt]]. */
  def pointAt(
      index: LatticeIndex[D]
  ): Either[GridDomainError, DomainIndex[S]] =
    domainIndexAt(index)

  /** Recover the spatial coordinate of an index owned by this live domain. */
  def indexOf(
      index: DomainIndex[S]
  )(using Dimension[D]): Either[GridDomainError, LatticeIndex[D]] =
    indexOfOrdinal(index.ordinal)

  /** Require the exact live grid owner, not equal serialized geometry. */
  def validateGrid[G <: Frame[D]](
      candidate: Grid[G, D]
  ): Either[GridDomainError, Unit] =
    if grid.sameRuntimeOwnerAs(candidate) then Right(())
    else
      Left(
        GridDomainError.GridRuntimeOwnerMismatch(
          persistentGridRecord.key.id
        )
      )

  /** Require the exact live domain owner, not equal persistent identity. */
  def validateSpace[T](
      candidate: FiniteSpace[T]
  ): Either[GridDomainError, Unit] =
    if space.sameRuntimeOwnerAs(candidate) then Right(())
    else
      Left(
        GridDomainError.DomainRuntimeOwnerMismatch(
          space.record,
          candidate.record
        )
      )

  /** Expose a spatial-only image as a zero-copy locus field. */
  def spatialField[
      I <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      image: Sampled[I, A, Sem, R]
  ): Either[
    GridDomainError,
    SpatialFieldView[S, I, A, Sem, R]
  ] =
    if !grid.sameRuntimeOwnerAs(image.grid) then
      Left(
        GridDomainError.ImageGridRuntimeOwnerMismatch(
          persistentGridRecord.key.id
        )
      )
    else if image.nonSpatialAxes.size != 0 then
      Left(
        GridDomainError.SpatialFieldRequiresNoNonSpatialAxes(
          image.nonSpatialAxes.size
        )
      )
    else Right(new SpatialFieldView(this, image))

  /** Expose each voxel's complete non-spatial series as a zero-copy field. */
  def seriesField[
      I <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      image: Sampled[I, A, Sem, R]
  ): Either[
    GridDomainError,
    SeriesFieldView[S, I, A, Sem, R]
  ] =
    if !grid.sameRuntimeOwnerAs(image.grid) then
      Left(
        GridDomainError.ImageGridRuntimeOwnerMismatch(
          persistentGridRecord.key.id
        )
      )
    else if image.nonSpatialAxes.size == 0 then
      Left(GridDomainError.SeriesFieldRequiresNonSpatialAxis)
    else Right(new SeriesFieldView(this, image))

  /** Translate an exact image view into a locus injection and, when complete,
    * a bijection.
    */
  def exactView(
      map: LatticeMap[D]
  ): Either[
    GridDomainError,
    GridDomainView[D, S]
  ] =
    GridDomain.exactView(this, map)

  private[locus] def coordinatesOfOrdinalUnchecked(
      ordinal: Int
  ): Vector[Int] =
    val coordinates = Array.ofDim[Int](grid.shape.length)
    var remaining = ordinal
    var axis = grid.shape.length - 1
    while axis >= 0 do
      val extent = grid.shape(axis)
      coordinates(axis) = remaining % extent
      remaining = remaining / extent
      axis -= 1
    coordinates.toVector

  private[locus] def ordinalOfCoordinatesUnchecked(
      coordinates: Vector[Int]
  ): Int =
    var ordinal = 0
    var axis = 0
    while axis < grid.shape.length do
      ordinal = ordinal * grid.shape(axis) + coordinates(axis)
      axis += 1
    ordinal

/** Exact target-to-source correspondence for a derived grid view. */
sealed trait GridDomainView[D <: Dim, S]:
  type T
  val map: LatticeMap[D]
  val target: FiniteDomain[T]
  val injection: Injection[T, S]
  val bijection: Option[Bijection[T, S]]

/** Fresh or restored bridge with a registry-created domain owner. */
sealed trait GridDomainResolution[
    F <: Frame[D],
    D <: Dim
]:
  type S
  val registry: DomainRegistry
  val value: GridDomain[F, D, S]

object GridDomain:
  val Layout: GridDomainLayout =
    GridDomainLayout.RowMajorLastAxisFastestV1

  /** Derive the one persistent locus key allowed for this grid and layout. */
  def canonicalDomainKey[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D]
  ): Either[GridDomainError, DomainKey] =
    for
      record <- persistentRecord(grid)
      count <- voxelCount(grid.shape)
      id <- DomainId
        .parse(s"image4s:grid-domain:${Layout.id}:${record.key.id.value}")
        .left
        .map(GridDomainError.DomainRecordFailure.apply)
      fingerprint <- DomainFingerprint
        .parse(canonicalFingerprint(record.key, Layout))
        .left
        .map(GridDomainError.DomainRecordFailure.apply)
      key <- DomainKey
        .make(id, count, Some(fingerprint))
        .left
        .map(GridDomainError.DomainRecordFailure.apply)
    yield key

  /** Derive a serializable locus record; `domainName` is presentation only. */
  def canonicalDomainRecord[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      domainName: String
  ): Either[GridDomainError, DomainRecord] =
    canonicalDomainKey(grid).flatMap: key =>
      DomainRecord
        .make(key, domainName)
        .left
        .map(GridDomainError.DomainRecordFailure.apply)

  /** Attach only when the supplied space has the grid's canonical key. */
  def attach[
      F <: Frame[D],
      D <: Dim,
      S
  ](
      grid: Grid[F, D],
      space: FiniteSpace[S]
  ): Either[GridDomainError, GridDomain[F, D, S]] =
    for
      gridRecord <- persistentRecord(grid)
      count <- voxelCount(grid.shape)
      _ <-
        Either.cond(
          space.size == count,
          (),
          GridDomainError.DomainSizeMismatch(count, space.size)
        )
      expected <- canonicalDomainKey(grid)
      _ <-
        Either.cond(
          space.key == expected,
          (),
          GridDomainError.DomainKeyMismatch(expected, space.key)
        )
    yield new GridDomain(grid, space, count, gridRecord)

  /** Register or converge on the canonical domain for a persistent grid. */
  def register[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      domainName: String,
      registry: DomainRegistry
  ): Either[GridDomainError, GridDomainResolution[F, D]] =
    canonicalDomainRecord(grid, domainName).flatMap: record =>
      registry
        .register(record)
        .left
        .map(GridDomainError.DomainRestoreFailure.apply)
        .flatMap(resolutionFrom(grid, _))

  /** Restore serialized grid-domain evidence through an immutable registry. */
  def restore[
      F <: Frame[D],
      D <: Dim
  ](
      record: GridDomainRecord,
      grid: Grid[F, D],
      registry: DomainRegistry
  ): Either[GridDomainError, GridDomainResolution[F, D]] =
    for
      _ <-
        Either.cond(
          record.layout == Layout,
          (),
          GridDomainError.LayoutMismatch(Layout, record.layout)
        )
      liveGridRecord <- persistentRecord(grid)
      _ <-
        Either.cond(
          record.grid == liveGridRecord,
          (),
          GridDomainError.GridRecordMismatch(
            record.grid,
            liveGridRecord
          )
        )
      expectedDomainKey <- canonicalDomainKey(grid)
      _ <-
        Either.cond(
          record.domain.key == expectedDomainKey,
          (),
          GridDomainError.DomainKeyMismatch(
            expectedDomainKey,
            record.domain.key
          )
        )
      resolution <- registry
        .restore(record.domain)
        .left
        .map(GridDomainError.DomainRestoreFailure.apply)
      bridge <- resolutionFrom(grid, resolution)
    yield bridge

  private final class Resolved[
      F <: Frame[D],
      D <: Dim,
      A
  ](
      val registry: DomainRegistry,
      val value: GridDomain[F, D, A]
  ) extends GridDomainResolution[F, D]:
    type S = A

  private final class DerivedView[
      D <: Dim,
      S,
      A
  ](
      val map: LatticeMap[D],
      val target: FiniteDomain[A],
      val injection: Injection[A, S],
      val bijection: Option[Bijection[A, S]]
  ) extends GridDomainView[D, S]:
    type T = A

  private def resolutionFrom[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      resolution: DomainResolution
  ): Either[GridDomainError, GridDomainResolution[F, D]] =
    attach(grid, resolution.space).map: bridge =>
      new Resolved(resolution.registry, bridge)

  private def exactView[
      F <: Frame[D],
      D <: Dim,
      S
  ](
      source: GridDomain[F, D, S],
      map: LatticeMap[D]
  ): Either[
    GridDomainError,
    GridDomainView[D, S]
  ] =
    if map.sourceShape != source.grid.shape then
      Left(
        GridDomainError.ImageFailure(
          ImageError.LatticeMapSourceShapeMismatch(
            source.grid.shape,
            map.sourceShape
          )
        )
      )
    else
      for
        targetCount <- voxelCount(map.targetShape)
        packed <- FiniteDomain
          .ephemeral(
            s"${source.space.name.value} exact view",
            targetCount
          )
          .left
          .map(GridDomainError.DomainRecordFailure.apply)
        result <- exactViewForTarget(source, map, packed.value)
      yield result

  private def exactViewForTarget[
      F <: Frame[D],
      D <: Dim,
      S,
      T
  ](
      source: GridDomain[F, D, S],
      map: LatticeMap[D],
      target: FiniteDomain[T]
  ): Either[
    GridDomainError,
    GridDomainView[D, S]
  ] =
    val targets = Array.ofDim[Int](target.size)
    var ordinal = 0
    var failure = Option.empty[GridDomainError]
    while ordinal < target.size && failure.isEmpty do
      val targetCoordinates =
        coordinatesOfOrdinalUnchecked(map.targetShape, ordinal)
      map.sourceIndex(targetCoordinates) match
        case Left(error) =>
          failure = Some(GridDomainError.ImageFailure(error))
        case Right(sourceCoordinates) =>
          targets(ordinal) =
            source.ordinalOfCoordinatesUnchecked(sourceCoordinates)
      ordinal += 1

    failure match
      case Some(error) =>
        Left(error)
      case None =>
        Injection
          .fromTargetOrdinals(target, source.space, targets)
          .left
          .map(locusMapError)
          .flatMap: injection =>
            val bijection =
              if target.size == source.space.size then
                Bijection
                  .fromTotalMap(injection.toTotalMap)
                  .toOption
              else None
            Right(
              new DerivedView(
                map,
                target,
                injection,
                bijection
              )
            )

  private def locusMapError(
      error: TotalMapError | CertifiedMapError
  ): GridDomainError =
    GridDomainError.LocusMapFailure(
      error match
        case value: TotalMapError     => value.message
        case value: CertifiedMapError => value.message
    )

  private def persistentRecord[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D]
  ): Either[GridDomainError, GridRecord] =
    grid.record.left.map(GridDomainError.PersistentGridRequired.apply)

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

  private def coordinatesOfOrdinalUnchecked(
      shape: Vector[Int],
      ordinal: Int
  ): Vector[Int] =
    val coordinates = Array.ofDim[Int](shape.length)
    var remaining = ordinal
    var axis = shape.length - 1
    while axis >= 0 do
      coordinates(axis) = remaining % shape(axis)
      remaining = remaining / shape(axis)
      axis -= 1
    coordinates.toVector

  private def canonicalFingerprint(
      key: GridKey,
      layout: GridDomainLayout
  ): String =
    Vector(
      component("schema", "image4s-grid-domain-key/v1"),
      component("layout", layout.id),
      component("grid-id", key.id.value),
      component("frame-id", key.frame.id.value),
      component("rank", key.spatialRank.toString),
      component("unit", key.frame.unit.toString),
      component("convention", key.frame.convention.toString),
      component("shape", key.shape.mkString(",")),
      component(
        "affine-bits",
        key.indexToFrame.rowMajor.map(encodeDouble).mkString(",")
      )
    ).mkString("|")

  private def component(name: String, value: String): String =
    s"$name:${value.length}:$value"

  private def encodeDouble(value: Double): String =
    val unpadded =
      java.lang.Long.toHexString(
        java.lang.Double.doubleToRawLongBits(value)
      )
    "0" * (16 - unpadded.length) + unpadded
