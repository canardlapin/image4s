package image4s

import ravel.AnyRank
import ravel.BorrowedNDArray
import ravel.BroadcastRank
import ravel.CanDropAxis
import ravel.ConversionPolicy
import ravel.DType
import ravel.DropAxis
import ravel.MutableNDArray
import ravel.NDArray
import ravel.NumericDType
import ravel.Rank
import ravel.map
import ravel.select
import ravel.zipMapExact
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid

final class Sampled[
    S <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] private (
    val data: NDArray[A, R],
    val sampleSpace: S,
    val metadata: ImageMetadata,
    private[image4s] val valueSemantics: ValueSemantics[A, Sem]
):
  inline def grid: Grid[sampleSpace.F, sampleSpace.D] =
    sampleSpace.grid

  inline def nonSpatialAxes: NonSpatialAxes =
    sampleSpace.nonSpatialAxes

  /** Physical element representation owned by Ravel. */
  inline def dtype: DType[A] =
    data.dtype

  val frame: sampleSpace.F =
    grid.frame

  val logicalShape: Vector[Int] =
    sampleSpace.logicalShape

  def withMetadata(
      next: ImageMetadata
  ): Sampled[S, A, Sem, R] =
    if next == metadata then this
    else new Sampled(data, sampleSpace, next, valueSemantics)

  def valueAt(
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[ImageError, A] =
    validateIndices(spatialIndex, nonSpatialIndex).map { _ =>
      readValidated(spatialIndex, nonSpatialIndex)
    }

  /** Check a runtime-known storage rank without copying data.
    *
    * Use this after loading a dynamically ranked image when later code needs a ranked `apply`
    * method or a statically ranked view.
    */
  def requireDataRank[N <: Int](using
      expected: ValueOf[N]
  ): Either[
    ImageError,
    Sampled[S, A, Sem, Rank[N]]
  ] =
    data
      .requireRank[N]
      .left
      .map(error => ImageError.StorageRankMismatch(error.expected, error.actual))
      .map(ranked => new Sampled(ranked, sampleSpace, metadata, valueSemantics))

  /** Fix one non-spatial coordinate and remove that axis.
    *
    * `axis` is relative to `nonSpatialAxes`, not to the complete Ravel shape. The result shares
    * immutable storage with this image and keeps the same spatial grid.
    */
  def selectNonSpatial(
      axis: Int,
      index: Int
  )(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
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
            metadata,
            valueSemantics
          )
        )

  /** Select the sole non-spatial axis of `kind`.
    *
    * The method rejects missing or repeated kinds instead of choosing an axis by position.
    */
  def selectAxis(
      kind: AxisKind,
      index: Int
  )(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    nonSpatialAxes
      .uniqueIndexOf(kind)
      .flatMap(selectNonSpatial(_, index))

  def selectTime(
      index: Int
  )(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    selectAxis(AxisKind.Time, index)

  /** Approachable spelling for selecting the sole declared time axis. */
  def atTime(
      index: Int
  )(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    selectTime(index)

  def selectChannel(
      index: Int
  )(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    selectAxis(AxisKind.Channel, index)

  def selectDirection(
      index: Int
  )(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    selectAxis(AxisKind.Direction, index)

  /** Apply an exact spatial pullback without copying stored values.
    *
    * The target grid affine is derived by composing the source grid geometry with the map. Only
    * maps representable by signed Ravel strides and an axis permutation can be constructed.
    */
  def view(
      map: LatticeMap[sampleSpace.D]
  )(using
      dimension: Dimension[sampleSpace.D]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    if map.sourceShape != grid.shape then
      Left(
        ImageError.LatticeMapSourceShapeMismatch(
          grid.shape,
          map.sourceShape
        )
      )
    else if map.isIdentity then Right(widenedSpatialOwner)
    else
      map.targetGrid(grid).map { targetGrid =>
        new Sampled(
          map.applyView(data),
          SampleSpace.create(targetGrid, nonSpatialAxes),
          metadata,
          valueSemantics
        )
      }

  /** Return an affine-correct spatial crop that shares immutable storage.
    *
    * `origin` and `shape` use grid-axis order. The returned grid has a fresh identity in the same
    * frame, and its index origin maps to this grid's `origin`. Non-spatial axes are unchanged.
    */
  def spatialView(
      origin: Vector[Int],
      shape: Vector[Int]
  )(using
      dimension: Dimension[sampleSpace.D]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    for
      map <- LatticeMap.crop[sampleSpace.D](grid.shape, origin, shape)
      result <- view(map)
    yield result

  /** Approachable spelling for an affine-correct zero-copy spatial crop. */
  def crop(
      origin: Vector[Int],
      shape: Vector[Int]
  )(using
      dimension: Dimension[sampleSpace.D]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    spatialView(origin, shape)

  def flipSpatial(
      axis: Int
  )(using
      dimension: Dimension[sampleSpace.D]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    LatticeMap
      .flip[sampleSpace.D](grid.shape, axis)
      .flatMap(view)

  def permuteSpatial(
      sourceAxisForTarget: IterableOnce[Int]
  )(using
      dimension: Dimension[sampleSpace.D]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    LatticeMap
      .permute[sampleSpace.D](grid.shape, sourceAxisForTarget)
      .flatMap(view)

  def strideSpatial(
      steps: IterableOnce[Int]
  )(using
      dimension: Dimension[sampleSpace.D]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    LatticeMap
      .stride[sampleSpace.D](grid.shape, steps)
      .flatMap(view)

  /** Reorder non-spatial sampling axes and their Ravel axes together. */
  def permuteNonSpatial(
      order: IterableOnce[Int]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      A,
      Sem,
      R
    ]
  ] =
    val copied = order.iterator.toVector
    nonSpatialAxes.permute(copied).map { targetAxes =>
      if copied == nonSpatialAxes.values.indices.toVector then widenedSpatialOwner
      else
        val spatial = Vector.range(0, grid.spatialRank)
        val trailing = copied.map(_ + grid.spatialRank)
        new Sampled(
          data.permuteAxes((spatial ++ trailing)*),
          SampleSpace.create(grid, targetAxes),
          metadata,
          valueSemantics
        )
    }

  /** Map the codomain through Ravel's shape-preserving kernel.
    *
    * The output semantic tag is always supplied by explicit evidence.
    */
  def mapValuesAs[B, OutSem](
      transform: A => B
  )(using
      outputSemantics: ValueSemantics[B, OutSem],
      dtype: DType[B]
  ): Sampled[S, B, OutSem, R] =
    new Sampled(
      data.map(transform),
      sampleSpace,
      metadata,
      outputSemantics
    )

  /** Map values without changing their element type or proven semantics. */
  def mapValues(
      transform: A => A
  )(using dtype: DType[A]): Sampled[S, A, Sem, R] =
    new Sampled(
      data.map(transform),
      sampleSpace,
      metadata,
      valueSemantics
    )

  /** Convert numeric storage without changing the sampled space, metadata, or semantic role.
    *
    * Ravel validates `Overflow.Reject` before allocating its output and executes the successful
    * conversion through a primitive storage kernel.
    */
  def convertTo[B](
      policy: ConversionPolicy = ConversionPolicy()
  )(using
      source: NumericDType[A],
      target: NumericDType[B],
      outputSemantics: ValueSemantics[B, Sem]
  ): Either[ImageError, Sampled[S, B, Sem, R]] =
    data
      .convert[B](policy)
      .left
      .map(ImageError.NumericConversion.apply)
      .map(converted =>
        new Sampled(
          converted,
          sampleSpace,
          metadata,
          outputSemantics
        )
      )

  /** Replace storage while rechecking the complete logical shape. */
  def replaceDataChecked[
      B,
      OutSem,
      R2 <: AnyRank
  ](
      nextData: NDArray[B, R2]
  )(using
      outputSemantics: ValueSemantics[B, OutSem]
  ): Either[ImageError, Sampled[S, B, OutSem, R2]] =
    val actual = shapeOf(nextData)
    if actual == logicalShape then
      Right(
        new Sampled(
          nextData,
          sampleSpace,
          metadata,
          outputSemantics
        )
      )
    else Left(ImageError.SampledShapeMismatch(logicalShape, actual))

  /** Attach a Ravel reduction result after removing one non-spatial axis.
    *
    * image4s updates and validates the sampling space; Ravel or a downstream algebra remains
    * responsible for the actual reducer and output storage.
    */
  def replaceAfterNonSpatialReduction[
      B,
      OutSem,
      R2 <: AnyRank
  ](
      axis: Int,
      reducedData: NDArray[B, R2]
  )(using
      outputSemantics: ValueSemantics[B, OutSem]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      B,
      OutSem,
      R2
    ]
  ] =
    nonSpatialAxes.remove(axis).flatMap { targetAxes =>
      val targetSpace = SampleSpace.create(grid, targetAxes)
      val actual = shapeOf(reducedData)
      if actual == targetSpace.logicalShape then
        Right(
          new Sampled(
            reducedData,
            targetSpace,
            metadata,
            outputSemantics
          )
        )
      else
        Left(
          ImageError.SampledShapeMismatch(
            targetSpace.logicalShape,
            actual
          )
        )
    }

  /** Return this image when its Ravel layout is canonical; otherwise copy its logical values into
    * canonical C order.
    */
  def canonicalLayout: Sampled[S, A, Sem, R] =
    if data.isCanonicalLayout then this
    else new Sampled(data.copy, sampleSpace, metadata, valueSemantics)

  /** Copy all logical values into a new canonical Ravel buffer. */
  def materializedCopy: Sampled[S, A, Sem, R] =
    new Sampled(data.copy, sampleSpace, metadata, valueSemantics)

  final override def equals(other: Any): Boolean =
    other match
      case reference: AnyRef => this eq reference
      case _ => false

  final override def hashCode(): Int =
    System.identityHashCode(this)

  def sameRuntimeSpaceAs(
      that: Sampled[?, ?, ?, ?]
  ): Boolean =
    sampleSpace.sameRuntimeSpaceAs(that.sampleSpace)

  def samePersistentSpaceAs(
      that: Sampled[?, ?, ?, ?]
  ): Either[ImageError, Boolean] =
    sampleSpace.samePersistentSpaceAs(that.sampleSpace)

  def sharesStorageWith(
      that: Sampled[?, ?, ?, ?]
  ): StorageSharing =
    if data eq that.data then StorageSharing.SameArrayObject
    else StorageSharing.Unknown

  def sameValuesAs[B](
      that: Sampled[?, B, ?, ?]
  )(
      equal: (A, B) => Boolean
  ): Boolean =
    if logicalShape != that.logicalShape then false
    else
      val left = data.elementsIterator
      val right = that.data.elementsIterator
      var same = true
      while same && left.hasNext && right.hasNext do same = equal(left.next(), right.next())
      same && !left.hasNext && !right.hasNext

  def rebind[T <: SampleSpace[?, ?]](
      alignment: SamplingAlignment[S, T]
  ): Sampled[T, A, Sem, R] =
    new Sampled(
      data,
      alignment.right,
      metadata,
      valueSemantics
    )

  /** Combine two values with the same exact static sample-space owner.
    *
    * The output semantic tag is selected explicitly. Descriptive metadata is retained from the left
    * operand. The rank reduces to the same concrete `Rank[N]` (or `AnyRank`); generic code sees
    * Ravel's truthful `BroadcastRank[R, R]` rather than relying on a cast.
    */
  def zipWithAs[
      OtherSem,
      C,
      OutSem
  ](
      that: Sampled[S, A, OtherSem, R]
  )(
      combine: (A, A) => C
  )(using
      outputSemantics: ValueSemantics[C, OutSem],
      dtype: DType[C]
  ): Sampled[S, C, OutSem, BroadcastRank[R, R]] =
    new Sampled(
      zipData(that, combine),
      sampleSpace,
      metadata,
      outputSemantics
    )

  /** Combine fields with the same exact owner, element type, and semantics. */
  def zipWith(
      that: Sampled[S, A, Sem, R]
  )(
      combine: (A, A) => A
  )(using dtype: DType[A]): Sampled[S, A, Sem, BroadcastRank[R, R]] =
    new Sampled(
      zipData(that, combine),
      sampleSpace,
      metadata,
      valueSemantics
    )

  /** Combine after one reusable exact alignment check.
    *
    * No sampling validation is repeated here; the output remains owned by the left sample space.
    */
  def zipWithAlignedAs[
      T <: SampleSpace[?, ?],
      OtherSem,
      C,
      OutSem
  ](
      that: Sampled[T, A, OtherSem, R],
      alignment: SamplingAlignment[S, T]
  )(
      combine: (A, A) => C
  )(using
      outputSemantics: ValueSemantics[C, OutSem],
      dtype: DType[C]
  ): Sampled[S, C, OutSem, BroadcastRank[R, R]] =
    val rebound = that.rebind(alignment.reverse)
    new Sampled(
      zipData(rebound, combine),
      sampleSpace,
      metadata,
      outputSemantics
    )

  /** Combine equal-typed fields after one reusable exact alignment check. */
  def zipWithAligned[
      T <: SampleSpace[?, ?]
  ](
      that: Sampled[T, A, Sem, R],
      alignment: SamplingAlignment[S, T]
  )(
      combine: (A, A) => A
  )(using dtype: DType[A]): Sampled[S, A, Sem, BroadcastRank[R, R]] =
    val rebound = that.rebind(alignment.reverse)
    new Sampled(
      zipData(rebound, combine),
      sampleSpace,
      metadata,
      valueSemantics
    )

  private def zipData[
      T <: SampleSpace[?, ?],
      OtherSem,
      C
  ](
      that: Sampled[T, A, OtherSem, R],
      combine: (A, A) => C
  )(using dtype: DType[C]): NDArray[C, BroadcastRank[R, R]] =
    data.zipMapExact(that.data)(combine)

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
        case (axis, coordinate) if coordinate < 0 || coordinate >= axis.extent =>
          ImageError.NonSpatialIndexOutOfBounds(
            axis.name,
            coordinate,
            axis.extent
          )
      } match
        case Some(error) => Left(error)
        case None => Right(())

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
        case ((extent, coordinate), axis) if coordinate < 0 || coordinate >= extent =>
          ImageError.SpatialIndexOutOfBounds(axis, coordinate, extent)
      } match
        case Some(error) => Left(error)
        case None => validateNonSpatialIndex(nonSpatialIndex)

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

  private def shapeOf[B, R2 <: AnyRank](
      array: NDArray[B, R2]
  ): Vector[Int] =
    Vector.tabulate(array.shape.rank)(array.shape.apply)

  /** Widen only the sample-space path while retaining its exact members.
    *
    * This allocates only a small immutable image header and retains the exact sample-space and
    * Ravel owners. It never copies sample storage.
    */
  private def widenedSpatialOwner: Sampled[
    ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
    A,
    Sem,
    R
  ] =
    new Sampled(data, sampleSpace, metadata, valueSemantics)

object Sampled:
  def create[
      A,
      Sem,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R]
  )(using
      ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    Sampled[sampleSpace.type, A, Sem, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, ImageMetadata.empty)

  def create[
      A,
      Sem,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R],
      metadata: ImageMetadata
  )(using
      ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    Sampled[sampleSpace.type, A, Sem, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, metadata)

  def create[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R]
  )(using
      ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, R]
  ] =
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
      Sem,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata
  )(using
      ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def continuous[A, R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R],
      metadata: ImageMetadata
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    ImageError,
    ContinuousImage[sampleSpace.type, A, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, metadata)

  def continuous[A, R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R]
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    ImageError,
    ContinuousImage[sampleSpace.type, A, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, ImageMetadata.empty)

  def continuous[F <: Frame[D], D <: Dim, A, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    ImageError,
    ContinuousImage[? <: SampleSpace[F, D], A, R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def categorical[A, R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R],
      metadata: ImageMetadata
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    ImageError,
    CategoricalImage[sampleSpace.type, A, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, metadata)

  def categorical[A, R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R]
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    ImageError,
    CategoricalImage[sampleSpace.type, A, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, ImageMetadata.empty)

  def categorical[F <: Frame[D], D <: Dim, A, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    ImageError,
    CategoricalImage[? <: SampleSpace[F, D], A, R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def copyContinuousFromMutable[
      F <: Frame[D],
      D <: Dim,
      A,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: MutableNDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    ImageError,
    ContinuousImage[? <: SampleSpace[F, D], A, R]
  ] =
    validateAndCreate(
      grid,
      nonSpatialAxes,
      data.freezeCopy(),
      metadata
    )

  def copyCategoricalFromMutable[
      F <: Frame[D],
      D <: Dim,
      A,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: MutableNDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    ImageError,
    CategoricalImage[? <: SampleSpace[F, D], A, R]
  ] =
    validateAndCreate(
      grid,
      nonSpatialAxes,
      data.freezeCopy(),
      metadata
    )

  def copyContinuousFromBorrowed[
      F <: Frame[D],
      D <: Dim,
      A,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: BorrowedNDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    ImageError,
    ContinuousImage[? <: SampleSpace[F, D], A, R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data.copy, metadata)

  def copyCategoricalFromBorrowed[
      F <: Frame[D],
      D <: Dim,
      A,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: BorrowedNDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    ImageError,
    CategoricalImage[? <: SampleSpace[F, D], A, R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data.copy, metadata)

  def mask[R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[Boolean, R],
      metadata: ImageMetadata
  )(using
      ValueSemantics[Boolean, Mask]
  ): Either[
    ImageError,
    MaskImage[sampleSpace.type, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, metadata)

  def mask[R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[Boolean, R]
  )(using
      ValueSemantics[Boolean, Mask]
  ): Either[
    ImageError,
    MaskImage[sampleSpace.type, R]
  ] =
    validateSpaceAndCreate(sampleSpace, data, ImageMetadata.empty)

  def mask[F <: Frame[D], D <: Dim, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[Boolean, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, Mask]
  ): Either[
    ImageError,
    MaskImage[? <: SampleSpace[F, D], R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data, metadata)

  def copyMaskFromMutable[F <: Frame[D], D <: Dim, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: MutableNDArray[Boolean, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, Mask]
  ): Either[
    ImageError,
    MaskImage[? <: SampleSpace[F, D], R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data.freezeCopy(), metadata)

  def copyMaskFromBorrowed[F <: Frame[D], D <: Dim, R <: AnyRank](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: BorrowedNDArray[Boolean, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, Mask]
  ): Either[
    ImageError,
    MaskImage[? <: SampleSpace[F, D], R]
  ] =
    validateAndCreate(grid, nonSpatialAxes, data.copy, metadata)

  private def validateAndCreate[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      R <: AnyRank
  ](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata
  )(using
      semantics: ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, R]
  ] =
    validateSpaceAndCreate(
      SampleSpace.create(grid, nonSpatialAxes),
      data,
      metadata
    )

  private def validateSpaceAndCreate[A, Sem, R <: AnyRank](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R],
      metadata: ImageMetadata
  )(using
      semantics: ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    Sampled[sampleSpace.type, A, Sem, R]
  ] =
    val expected = sampleSpace.logicalShape
    val actual =
      Vector.tabulate(data.shape.rank)(data.shape.apply)
    if actual == expected then Right(new Sampled(data, sampleSpace, metadata, semantics))
    else Left(ImageError.SampledShapeMismatch(expected, actual))

type Image[
    S <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] = Sampled[S, A, Sem, R]

/** Constructor facade; [[Sampled]] remains the only image representation. */
object Image:
  export Sampled.{
    categorical,
    continuous,
    copyCategoricalFromBorrowed,
    copyCategoricalFromMutable,
    copyContinuousFromBorrowed,
    copyContinuousFromMutable,
    copyMaskFromBorrowed,
    copyMaskFromMutable,
    create,
    mask
  }

type ContinuousImage[
    S <: SampleSpace[?, ?],
    A,
    R <: AnyRank
] = Sampled[S, A, Continuous, R]

type CategoricalImage[
    S <: SampleSpace[?, ?],
    A,
    R <: AnyRank
] = Sampled[S, A, Categorical, R]

type MaskImage[
    S <: SampleSpace[?, ?],
    R <: AnyRank
] = Sampled[S, Boolean, Mask, R]

type DoubleContinuousImage[
    S <: SampleSpace[?, ?],
    R <: AnyRank
] = ContinuousImage[S, Double, R]

type FloatContinuousImage[
    S <: SampleSpace[?, ?],
    R <: AnyRank
] = ContinuousImage[S, Float, R]

/** Allocation-free logical indexing for statically ranked sampled values.
  *
  * Arguments follow the complete logical axis order: spatial grid axes first, then declared
  * non-spatial axes. Bounds and arity failures use Ravel's ranked indexing errors.
  */
extension [
    S <: SampleSpace[?, ?],
    A,
    Sem
](sampled: Sampled[S, A, Sem, Rank[2]])
  inline def apply(i0: Int, i1: Int): A =
    sampled.data(i0, i1)

extension [
    S <: SampleSpace[?, ?],
    A,
    Sem
](sampled: Sampled[S, A, Sem, Rank[3]])
  inline def apply(i0: Int, i1: Int, i2: Int): A =
    sampled.data(i0, i1, i2)

extension [
    S <: SampleSpace[?, ?],
    A,
    Sem
](sampled: Sampled[S, A, Sem, Rank[4]])
  inline def apply(i0: Int, i1: Int, i2: Int, i3: Int): A =
    sampled.data(i0, i1, i2, i3)
