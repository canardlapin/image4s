package image4s

import ravel.AnyRank
import ravel.BorrowedNDArray
import ravel.CanDropAxis
import ravel.DropAxis
import ravel.MutableNDArray
import ravel.NDArray
import ravel.Rank
import ravel.select
import image4s.geometry.Affine
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid

sealed trait FieldRole
sealed trait Continuous extends FieldRole
sealed trait Scalar extends Continuous
sealed trait Components extends Continuous
sealed trait Label extends FieldRole

final class Sampled[
    F <: Frame[D],
    D <: Dim,
    A,
    Role <: FieldRole,
    R <: AnyRank
] private (
    val data: NDArray[A, R],
    val sampleSpace: SampleSpace[F, D],
    val metadata: ImageMetadata
):
  inline def grid: Grid[F, D] =
    sampleSpace.grid

  inline def nonSpatialAxes: NonSpatialAxes =
    sampleSpace.nonSpatialAxes

  val frame: F =
    grid.frame

  val logicalShape: Vector[Int] =
    sampleSpace.logicalShape

  def withMetadata(
      next: ImageMetadata
  ): Sampled[F, D, A, Role, R] =
    if next == metadata then this
    else new Sampled(data, sampleSpace, next)

  def valueAt(
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[ImageError, A] =
    validateIndices(spatialIndex, nonSpatialIndex).map { _ =>
      readValidated(spatialIndex, nonSpatialIndex)
    }

  /** Check a runtime-known storage rank without copying data.
    *
    * Use this after loading a dynamically ranked image when later code needs a
    * ranked `apply` method or a statically ranked view.
    */
  def requireDataRank[N <: Int](using
      expected: ValueOf[N]
  ): Either[
    ImageError,
    Sampled[F, D, A, Role, Rank[N]]
  ] =
    data
      .requireRank[N]
      .left
      .map(error =>
        ImageError.StorageRankMismatch(error.expected, error.actual)
      )
      .map(ranked => new Sampled(ranked, sampleSpace, metadata))

  /** Fix one non-spatial coordinate and remove that axis.
    *
    * `axis` is relative to `nonSpatialAxes`, not to the complete Ravel shape.
    * The result shares immutable storage with this image and keeps the same
    * spatial grid.
    */
  def selectNonSpatial(
      axis: Int,
      index: Int
  )(using CanDropAxis[R]): Either[
    ImageError,
    Sampled[F, D, A, Role, DropAxis[R]]
  ] =
    nonSpatialAxes(axis) match
      case None =>
        Left(
          ImageError.NonSpatialAxisOutOfBounds(
            axis,
            nonSpatialAxes.size
          )
        )
      case Some(selected) if index < 0 || index >= selected.extent =>
        Left(
          ImageError.NonSpatialIndexOutOfBounds(
            selected.name,
            index,
            selected.extent
          )
        )
      case Some(_) =>
        val dataAxis = grid.spatialRank + axis
        Right(
          new Sampled(
            data.select(dataAxis, index),
            SampleSpace.create(grid, nonSpatialAxes.without(axis)),
            metadata
          )
        )

  /** Select the sole non-spatial axis of `kind`.
    *
    * The method rejects missing or repeated kinds instead of choosing an axis
    * by position.
    */
  def selectAxis(
      kind: AxisKind,
      index: Int
  )(using CanDropAxis[R]): Either[
    ImageError,
    Sampled[F, D, A, Role, DropAxis[R]]
  ] =
    var found = -1
    var count = 0
    var axis = 0
    while axis < nonSpatialAxes.values.length do
      if nonSpatialAxes.values(axis).kind == kind then
        found = axis
        count += 1
      axis += 1
    if count == 0 then Left(ImageError.MissingNonSpatialAxisKind(kind))
    else if count > 1 then
      Left(ImageError.AmbiguousNonSpatialAxisKind(kind, count))
    else selectNonSpatial(found, index)

  def selectTime(
      index: Int
  )(using CanDropAxis[R]): Either[
    ImageError,
    Sampled[F, D, A, Role, DropAxis[R]]
  ] =
    selectAxis(AxisKind.Time, index)

  def selectChannel(
      index: Int
  )(using CanDropAxis[R]): Either[
    ImageError,
    Sampled[F, D, A, Role, DropAxis[R]]
  ] =
    selectAxis(AxisKind.Channel, index)

  def selectDirection(
      index: Int
  )(using CanDropAxis[R]): Either[
    ImageError,
    Sampled[F, D, A, Role, DropAxis[R]]
  ] =
    selectAxis(AxisKind.Direction, index)

  /** Return an affine-correct spatial crop that shares immutable storage.
    *
    * `origin` and `shape` use grid-axis order. The returned grid has a fresh
    * identity in the same frame, and its index origin maps to this grid's
    * `origin`. Non-spatial axes are unchanged.
    */
  def spatialView(
      origin: Vector[Int],
      shape: Vector[Int]
  )(using dimension: Dimension[D]): Either[
    ImageError,
    Sampled[F, D, A, Role, R]
  ] =
    for
      _ <- validateSpatialView(origin, shape)
      shiftedOrigin <- grid.indexToFrame
        .apply(origin.map(_.toDouble))
        .left
        .map(ImageError.Geometry.apply)
      shiftedAffine <- Affine
        .fromRowMajor[D](
          shiftedAffineValues(shiftedOrigin),
          grid.indexToFrame.tolerance
        )
        .left
        .map(ImageError.Geometry.apply)
      viewGrid <- Grid
        .forFrame[D, F](grid.frame)(shape, shiftedAffine)
        .left
        .map(ImageError.Geometry.apply)
    yield new Sampled(
      narrowSpatial(origin, shape),
      SampleSpace.create(viewGrid, nonSpatialAxes),
      metadata
    )

  /** Return this image when its Ravel layout is canonical; otherwise copy its
    * logical values into canonical C order.
    */
  def canonicalLayout: Sampled[F, D, A, Role, R] =
    if data.isCanonicalLayout then this
    else new Sampled(data.copy, sampleSpace, metadata)

  /** Copy all logical values into a new canonical Ravel buffer. */
  def materializedCopy: Sampled[F, D, A, Role, R] =
    new Sampled(data.copy, sampleSpace, metadata)

  override def equals(other: Any): Boolean =
    other match
      case that: Sampled[?, ?, ?, ?, ?] =>
        (this eq that) ||
          (
            sampleSpace == that.sampleSpace &&
              metadata == that.metadata &&
              sameData(that)
          )
      case _ => false

  override def hashCode(): Int =
    var hash = sampleSpace.hashCode()
    hash = 31 * hash + metadata.hashCode()
    val iterator = data.elementsIterator
    while iterator.hasNext do
      hash = 31 * hash + iterator.next().##
    hash

  private def sameData(
      that: Sampled[?, ?, ?, ?, ?]
  ): Boolean =
    if data.shape != that.data.shape then false
    else
      val left = data.elementsIterator
      val right = that.data.elementsIterator
      var same = true
      while same && left.hasNext && right.hasNext do
        same = left.next() == right.next()
      same && !left.hasNext && !right.hasNext

  private[image4s] def validateNonSpatialIndex(
      index: Vector[Int]
  ): Either[ImageError, Unit] =
    if index.length != nonSpatialAxes.size then
      Left(
        ImageError.NonSpatialIndexRankMismatch(
          nonSpatialAxes.size,
          index.length
        )
      )
    else
      nonSpatialAxes.values.zip(index).collectFirst {
        case (axis, coordinate)
            if coordinate < 0 || coordinate >= axis.extent =>
          ImageError.NonSpatialIndexOutOfBounds(
            axis.name,
            coordinate,
            axis.extent
          )
      } match
        case Some(error) => Left(error)
        case None        => Right(())

  private def validateIndices(
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int]
  ): Either[ImageError, Unit] =
    if spatialIndex.length != grid.shape.length then
      Left(
        ImageError.SpatialIndexRankMismatch(
          grid.shape.length,
          spatialIndex.length
        )
      )
    else
      grid.shape.zip(spatialIndex).zipWithIndex.collectFirst {
        case ((extent, coordinate), axis)
            if coordinate < 0 || coordinate >= extent =>
          ImageError.SpatialIndexOutOfBounds(axis, coordinate, extent)
      } match
        case Some(error) => Left(error)
        case None        => validateNonSpatialIndex(nonSpatialIndex)

  private def readValidated(
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int]
  ): A =
    (spatialIndex.length, nonSpatialIndex.length) match
      case (2, 0) =>
        data(spatialIndex(0), spatialIndex(1))
      case (2, 1) =>
        data(
          spatialIndex(0),
          spatialIndex(1),
          nonSpatialIndex(0)
        )
      case (2, 2) =>
        data(
          spatialIndex(0),
          spatialIndex(1),
          nonSpatialIndex(0),
          nonSpatialIndex(1)
        )
      case (3, 0) =>
        data(spatialIndex(0), spatialIndex(1), spatialIndex(2))
      case (3, 1) =>
        data(
          spatialIndex(0),
          spatialIndex(1),
          spatialIndex(2),
          nonSpatialIndex(0)
        )
      case _ =>
        val indices = spatialIndex ++ nonSpatialIndex
        data.at(IArray.unsafeFromArray(indices.toArray))

  private def validateSpatialView(
      origin: Vector[Int],
      shape: Vector[Int]
  ): Either[ImageError, Unit] =
    if origin.length != grid.spatialRank ||
      shape.length != grid.spatialRank
    then
      Left(
        ImageError.SpatialViewRankMismatch(
          grid.spatialRank,
          origin.length,
          shape.length
        )
      )
    else
      shape.zipWithIndex.collectFirst {
        case (extent, axis) if extent <= 0 =>
          ImageError.NonPositiveSpatialViewExtent(axis, extent)
      }.orElse(
        origin.indices.collectFirst {
          case axis
              if origin(axis) < 0 ||
                origin(axis).toLong + shape(axis).toLong >
                  grid.shape(axis).toLong =>
            ImageError.SpatialViewOutOfBounds(
              axis,
              origin(axis),
              shape(axis),
              grid.shape(axis)
            )
        }
      ) match
        case Some(error) => Left(error)
        case None        => Right(())

  private def narrowSpatial(
      origin: Vector[Int],
      shape: Vector[Int]
  ): NDArray[A, R] =
    var view = data
    var axis = 0
    while axis < grid.spatialRank do
      view = view.narrow(axis, origin(axis), shape(axis))
      axis += 1
    view

  private def shiftedAffineValues(
      shiftedOrigin: Vector[Double]
  )(using dimension: Dimension[D]): Vector[Double] =
    val rank = dimension.rank
    val size = rank + 1
    val current = grid.indexToFrame.rowMajor
    Vector.tabulate(current.length) { flat =>
      val row = flat / size
      val column = flat % size
      if row < rank && column == rank then shiftedOrigin(row)
      else current(flat)
    }

object Sampled:
  def create[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[F, D],
      data: NDArray[A, R]
  ): Either[ImageError, Sampled[F, D, A, Role, R]] =
    validateAndCreate(sampleSpace, data, ImageMetadata.empty)

  def create[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[F, D],
      data: NDArray[A, R],
      metadata: ImageMetadata
  ): Either[ImageError, Sampled[F, D, A, Role, R]] =
    validateAndCreate(sampleSpace, data, metadata)

  def create[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R]
  ): Either[ImageError, Sampled[F, D, A, Role, R]] =
    validateAndCreate(
      grid,
      nonSpatialAxes,
      data,
      ImageMetadata.empty
    )

  def create[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata
  ): Either[ImageError, Sampled[F, D, A, Role, R]] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def scalar[F <: Frame[D], D <: Dim, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[Double, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, ScalarImage[F, D, R]] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def components[F <: Frame[D], D <: Dim, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[Double, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, ComponentImage[F, D, R]] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def labels[F <: Frame[D], D <: Dim, A, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, LabelImage[F, D, A, R]] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def copyScalarFromMutable[
      F <: Frame[D],
      D <: Dim,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: MutableNDArray[Double, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, ScalarImage[F, D, R]] =
    validateAndCreate(
      grid,
      nonSpatialAxes,
      data.freezeCopy(),
      metadata
    )

  def copyLabelsFromMutable[
      F <: Frame[D],
      D <: Dim,
      A,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: MutableNDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, LabelImage[F, D, A, R]] =
    validateAndCreate(
      grid,
      nonSpatialAxes,
      data.freezeCopy(),
      metadata
    )

  def copyScalarFromBorrowed[
      F <: Frame[D],
      D <: Dim,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: BorrowedNDArray[Double, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, ScalarImage[F, D, R]] =
    validateAndCreate(grid, nonSpatialAxes, data.copy, metadata)

  def copyLabelsFromBorrowed[
      F <: Frame[D],
      D <: Dim,
      A,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: BorrowedNDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[ImageError, LabelImage[F, D, A, R]] =
    validateAndCreate(grid, nonSpatialAxes, data.copy, metadata)

  private def validateAndCreate[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata
  ): Either[ImageError, Sampled[F, D, A, Role, R]] =
    validateAndCreate(
      SampleSpace.create(grid, nonSpatialAxes),
      data,
      metadata
    )

  private def validateAndCreate[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[F, D],
      data: NDArray[A, R],
      metadata: ImageMetadata
  ): Either[ImageError, Sampled[F, D, A, Role, R]] =
    val expected = sampleSpace.logicalShape
    val actual =
      Vector.tabulate(data.shape.rank)(data.shape.apply)
    if actual == expected then
      Right(new Sampled(data, sampleSpace, metadata))
    else Left(ImageError.SampledShapeMismatch(expected, actual))

type Image[
    F <: Frame[D],
    D <: Dim,
    A,
    Role <: FieldRole,
    R <: AnyRank
] = Sampled[F, D, A, Role, R]

type ImageSeries[
    F <: Frame[D],
    D <: Dim,
    A,
    Role <: FieldRole,
    R <: AnyRank
] = Sampled[F, D, A, Role, R]

type ScalarImage[
    F <: Frame[D],
    D <: Dim,
    R <: AnyRank
] = Sampled[F, D, Double, Scalar, R]

type ComponentImage[
    F <: Frame[D],
    D <: Dim,
    R <: AnyRank
] = Sampled[F, D, Double, Components, R]

type LabelImage[
    F <: Frame[D],
    D <: Dim,
    A,
    R <: AnyRank
] = Sampled[F, D, A, Label, R]

/** Allocation-free logical indexing for statically ranked sampled values.
  *
  * Arguments follow the complete logical axis order: spatial grid axes first,
  * then declared non-spatial axes. Bounds and arity failures use Ravel's
  * ranked indexing errors.
  */
extension [
    F <: Frame[D],
    D <: Dim,
    A,
    Role <: FieldRole
](sampled: Sampled[F, D, A, Role, Rank[2]])
  inline def apply(i0: Int, i1: Int): A =
    sampled.data(i0, i1)

extension [
    F <: Frame[D],
    D <: Dim,
    A,
    Role <: FieldRole
](sampled: Sampled[F, D, A, Role, Rank[3]])
  inline def apply(i0: Int, i1: Int, i2: Int): A =
    sampled.data(i0, i1, i2)

extension [
    F <: Frame[D],
    D <: Dim,
    A,
    Role <: FieldRole
](sampled: Sampled[F, D, A, Role, Rank[4]])
  inline def apply(i0: Int, i1: Int, i2: Int, i3: Int): A =
    sampled.data(i0, i1, i2, i3)
