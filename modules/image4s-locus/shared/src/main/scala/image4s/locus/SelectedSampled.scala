package image4s.locus

import image4s.Categorical
import image4s.Continuous
import image4s.ImageMetadata
import image4s.ImageError
import image4s.Mask
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.ValueSemantics
import image4s.geometry.Dim
import image4s.geometry.D3
import image4s.geometry.Frame
import locus4s.Index
import locus4s.Selection
import locus4s.SpaceMismatch
import locus4s.data.Field
import ravel.AnyRank
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import ravel.map
import ravel.select

enum SelectedSampledError:
  case Domain(error: GridDomainError)
  case SelectionSpace(error: SpaceMismatch)
  case DataShapeMismatch(expected: Vector[Int], actual: Vector[Int])
  case PositionOutOfBounds(position: Int, size: Int)
  case NonSpatialIndexCountMismatch(expected: Int, actual: Int)
  case NonSpatialIndexOutOfBounds(axis: Int, index: Int, extent: Int)
  case DataRankMismatch(expected: Int, actual: Int)
  case InvalidStorageShape(detail: String)
  case Image(error: ImageError)

  def message: String =
    this match
      case Domain(error) =>
        error.message
      case SelectionSpace(error) =>
        error.message
      case DataShapeMismatch(expected, actual) =>
        s"selected data requires shape ${expected.mkString("(", ", ", ")")}, " +
          s"found ${actual.mkString("(", ", ", ")")}"
      case PositionOutOfBounds(position, size) =>
        s"selection position is outside [0, $size): $position"
      case NonSpatialIndexCountMismatch(expected, actual) =>
        s"selected value access requires $expected non-spatial indices, found $actual"
      case NonSpatialIndexOutOfBounds(axis, index, extent) =>
        s"non-spatial index for axis $axis is outside [0, $extent): $index"
      case DataRankMismatch(expected, actual) =>
        s"selected data requires rank $expected, found $actual"
      case InvalidStorageShape(detail) =>
        s"selected storage shape is invalid: $detail"
      case Image(error) =>
        error.message

/** Compact sampled values on one exact ordered locus selection.
  *
  * The leading Ravel axis is always selection position. Remaining axes are the declared image4s
  * non-spatial axes in order. The selection is the sole support and ordering owner; no mask,
  * coordinate bag, or reverse lookup is retained alongside it.
  */
final class SelectedSampled[
    F <: Frame[D],
    D <: Dim,
    S,
    A,
    Sem,
    R <: AnyRank
] private (
    val domain: GridDomain[F, D, S],
    val selection: Selection[S],
    val nonSpatialAxes: NonSpatialAxes,
    val data: NDArray[A, R],
    val metadata: ImageMetadata,
    private[image4s] val valueSemantics: ValueSemantics[A, Sem]
):
  val logicalShape: Vector[Int] =
    selection.size +: nonSpatialAxes.shape

  inline def dtype: DType[A] =
    data.dtype

  def withMetadata(
      next: ImageMetadata
  ): SelectedSampled[F, D, S, A, Sem, R] =
    if next == metadata then this
    else
      new SelectedSampled(
        domain,
        selection,
        nonSpatialAxes,
        data,
        next,
        valueSemantics
      )

  def requireDataRank[N <: Int](using
      expected: ValueOf[N]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Sem, Rank[N]]
  ] =
    data
      .requireRank[N]
      .left
      .map(error =>
        SelectedSampledError.DataRankMismatch(
          error.expected,
          error.actual
        )
      )
      .map: ranked =>
        new SelectedSampled(
          domain,
          selection,
          nonSpatialAxes,
          ranked,
          metadata,
          valueSemantics
        )

  def valueAt(
      position: Index[selection.I],
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[SelectedSampledError, A] =
    validateNonSpatial(nonSpatialIndex).map: _ =>
      readUnchecked(position.ordinal, nonSpatialIndex)

  def valueAtPosition(
      position: Int,
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[SelectedSampledError, A] =
    if selection.positions.indexOption(position).isEmpty then
      Left(
        SelectedSampledError.PositionOutOfBounds(
          position,
          selection.size
        )
      )
    else
      validateNonSpatial(nonSpatialIndex).map: _ =>
        readUnchecked(position, nonSpatialIndex)

  /** A zero-copy field over the selection's path-dependent position domain. */
  def fieldAt(
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[SelectedSampledError, Field[selection.I, A]] =
    validateNonSpatial(nonSpatialIndex).map: _ =>
      Field.view(selection.positions): position =>
        readUnchecked(position.ordinal, nonSpatialIndex)

  def mapValues[B, OutSem](
      f: A => B
  )(using
      dtype: DType[B],
      semantics: ValueSemantics[B, OutSem]
  ): SelectedSampled[F, D, S, B, OutSem, R] =
    new SelectedSampled(
      domain,
      selection,
      nonSpatialAxes,
      data.map(f),
      metadata,
      semantics
    )

  /** Replace the ordered selection and its matching compact values.
    *
    * This is the checked primitive for filtering or reindexing selected data. It retains the grid
    * owner, axes, metadata, and stored value semantics; callers must supply exactly one value row
    * per position in `nextSelection`.
    */
  def withSelectionData[T, R2 <: AnyRank](
      nextSelection: Selection[T],
      nextData: NDArray[A, R2]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Sem, R2]
  ] =
    given ValueSemantics[A, Sem] = valueSemantics
    SelectedSampled.create(
      domain,
      nextSelection,
      nonSpatialAxes,
      nextData,
      metadata
    )

  /** Materialize a dense sampled image on this exact grid.
    *
    * Every point outside the selection receives `fill`. Selected values are scattered by canonical
    * grid ordinal, and the stored value-semantics evidence is retained without asking downstream
    * domain code to recreate it. This is the explicit allocation boundary from compact to dense
    * storage.
    */
  def scatter(
      fill: A
  ): Either[
    SelectedSampledError,
    Sampled[? <: SampleSpace[F, D], A, Sem, AnyRank]
  ] =
    for
      shape <- Shape
        .from(domain.grid.shape ++ nonSpatialAxes.shape)
        .left
        .map(error => SelectedSampledError.InvalidStorageShape(error.getMessage))
      dense = scatterValues(shape, fill)
      sampled <-
        given ValueSemantics[A, Sem] = valueSemantics
        Sampled
          .create(domain.grid, nonSpatialAxes, dense, metadata)
          .left
          .map(SelectedSampledError.Image.apply)
    yield sampled

  private def validateNonSpatial(
      indices: Vector[Int]
  ): Either[SelectedSampledError, Unit] =
    if indices.length != nonSpatialAxes.size then
      Left(
        SelectedSampledError.NonSpatialIndexCountMismatch(
          nonSpatialAxes.size,
          indices.length
        )
      )
    else
      var axis = 0
      var error = Option.empty[SelectedSampledError]
      while axis < indices.length && error.isEmpty do
        val extent = nonSpatialAxes.shape(axis)
        val index = indices(axis)
        if index < 0 || index >= extent then
          error = Some(
            SelectedSampledError.NonSpatialIndexOutOfBounds(
              axis,
              index,
              extent
            )
          )
        axis += 1
      error.toLeft(())

  private def readUnchecked(
      position: Int,
      nonSpatialIndex: Vector[Int]
  ): A =
    val indices = Array.ofDim[Int](1 + nonSpatialIndex.length)
    indices(0) = position
    var axis = 0
    while axis < nonSpatialIndex.length do
      indices(axis + 1) = nonSpatialIndex(axis)
      axis += 1
    data.at(IArray.unsafeFromArray(indices))

  private def scatterValues(
      shape: Shape[AnyRank],
      fill: A
  ): NDArray[A, AnyRank] =
    given DType[A] = data.dtype
    val trailingShape = nonSpatialAxes.shape
    val trailingSize =
      if trailingShape.isEmpty then 1
      else trailingShape.product
    NDArray.build[A, AnyRank](shape): output =>
      var denseOrdinal = 0
      while denseOrdinal < shape.size do
        output.writeLinear(denseOrdinal, fill)
        denseOrdinal += 1

      val compactValues = data.iterator
      var position = 0
      while position < selection.size do
        val compactIndex =
          selection.positions.indexAtValidatedOrdinal(position)
        val sourceOrdinal = selection(compactIndex).ordinal
        val base = sourceOrdinal * trailingSize
        var trailingOrdinal = 0
        while trailingOrdinal < trailingSize do
          output.writeLinear(
            base + trailingOrdinal,
            compactValues.next()
          )
          trailingOrdinal += 1
        position += 1

object SelectedSampled:
  def create[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      A,
      Sem,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      selection: Selection[T],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      semantics: ValueSemantics[A, Sem]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Sem, R]
  ] =
    exactSelectionOwner(domain, selection).flatMap: nativeSelection =>
      val expected = nativeSelection.size +: nonSpatialAxes.shape
      val actual = dimensions(data.shape)
      if actual == expected then
        Right(
          new SelectedSampled(
            domain,
            nativeSelection,
            nonSpatialAxes,
            data,
            metadata,
            semantics
          )
        )
      else
        Left(
          SelectedSampledError.DataShapeMismatch(expected, actual)
        )

  def continuous[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      A,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      selection: Selection[T],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Continuous, R]
  ] =
    create(domain, selection, nonSpatialAxes, data, metadata)

  def categorical[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      A,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      selection: Selection[T],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Categorical, R]
  ] =
    create(domain, selection, nonSpatialAxes, data, metadata)

  def mask[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      selection: Selection[T],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[Boolean, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, Mask]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, Boolean, Mask, R]
  ] =
    create(domain, selection, nonSpatialAxes, data, metadata)

  /** Gather all values into one canonical compact array.
    *
    * Selection position is the leading axis, so the complete trailing sample for one selected point
    * is contiguous in canonical storage.
    */
  def gather[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      I <: SampleSpace[?, D],
      A,
      Sem,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      image: Sampled[I, A, Sem, R],
      selection: Selection[T]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Sem, AnyRank]
  ] =
    for
      _ <- domain
        .validateGrid(image.grid)
        .left
        .map(SelectedSampledError.Domain.apply)
      nativeSelection <- exactSelectionOwner(domain, selection)
      shape <- Shape
        .from(nativeSelection.size +: image.nonSpatialAxes.shape)
        .left
        .map(error => SelectedSampledError.InvalidStorageShape(error.getMessage))
      compact = gatherValues(domain, image, nativeSelection, shape)
      selected <- create(
        domain,
        nativeSelection,
        image.nonSpatialAxes,
        compact,
        image.metadata
      )(using image.valueSemantics)
    yield selected

  def gatherSpatial[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      I <: SampleSpace[?, D],
      A,
      Sem,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      image: Sampled[I, A, Sem, R],
      selection: Selection[T]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Sem, Rank[1]]
  ] =
    gather(domain, image, selection).flatMap(_.requireDataRank[1])

  /** Allocation-disciplined D3 scalar gather.
    *
    * The D3/rank-3 contract permits primitive coordinate access without a
    * dynamic index array. One canonical compact Ravel destination is retained;
    * the exact selection remains the sole support and ordering owner.
    */
  def gatherVolume[
      F <: Frame[D3],
      S,
      T,
      I <: SampleSpace[?, D3],
      A,
      Sem
  ](
      domain: GridDomain[F, D3, S],
      image: Sampled[I, A, Sem, Rank[3]],
      selection: Selection[T]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D3, S, A, Sem, Rank[1]]
  ] =
    for
      _ <- domain
        .validateGrid(image.grid)
        .left
        .map(SelectedSampledError.Domain.apply)
      nativeSelection <- exactSelectionOwner(domain, selection)
      compact =
        given DType[A] = image.dtype
        val ny = domain.grid.shape(1)
        val nz = domain.grid.shape(2)
        val plane = ny * nz
        NDArray.build[A, Rank[1]](Shape(nativeSelection.size)):
          output =>
            var position = 0
            while position < nativeSelection.size do
              val selectedPosition =
                nativeSelection.positions.indexAtValidatedOrdinal(position)
              val ordinal = nativeSelection(selectedPosition).ordinal
              val x = ordinal / plane
              val withinPlane = ordinal % plane
              val y = withinPlane / nz
              val z = withinPlane % nz
              output.writeLinear(position, image.data(x, y, z))
              position += 1
      selected <- create(
        domain,
        nativeSelection,
        image.nonSpatialAxes,
        compact,
        image.metadata
      )(using image.valueSemantics)
    yield selected

  def gatherSingleAxis[
      F <: Frame[D],
      D <: Dim,
      S,
      T,
      I <: SampleSpace[?, D],
      A,
      Sem,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      image: Sampled[I, A, Sem, R],
      selection: Selection[T]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D, S, A, Sem, Rank[2]]
  ] =
    gather(domain, image, selection).flatMap(_.requireDataRank[2])

  extension [
      F <: Frame[D],
      D <: Dim,
      S,
      A,
      Sem
  ](
      selected: SelectedSampled[F, D, S, A, Sem, Rank[1]]
  )
    def field: Field[selected.selection.I, A] =
      Field.view(selected.selection.positions): position =>
        selected.data(position.ordinal)

  extension [
      F <: Frame[D3],
      S,
      A,
      Sem
  ](
      selected: SelectedSampled[F, D3, S, A, Sem, Rank[1]]
  )
    /** Allocation-disciplined scalar scatter to one canonical D3 Ravel
      * destination. The compact selection is reused and no dynamic-rank
      * iterator or output-sized staging representation is created.
      */
    def scatterVolume(
        fill: A
    ): Either[
      SelectedSampledError,
      Sampled[? <: SampleSpace[F, D3], A, Sem, Rank[3]]
    ] =
      given DType[A] = selected.data.dtype
      val spatialShape = selected.domain.grid.shape
      val dense =
        NDArray.build[A, Rank[3]](
          Shape(spatialShape(0), spatialShape(1), spatialShape(2))
        ): output =>
          var ordinal = 0
          while ordinal < selected.domain.space.size do
            output.writeLinear(ordinal, fill)
            ordinal += 1
          var position = 0
          while position < selected.selection.size do
            val selectedPosition =
              selected.selection.positions.indexAtValidatedOrdinal(position)
            val sourceOrdinal =
              selected.selection(selectedPosition).ordinal
            output.writeLinear(sourceOrdinal, selected.data(position))
            position += 1
      given ValueSemantics[A, Sem] = selected.valueSemantics
      Sampled
        .create(
          selected.domain.grid,
          NonSpatialAxes.empty,
          dense,
          selected.metadata
        )
        .left
        .map(SelectedSampledError.Image.apply)

  extension [
      F <: Frame[D],
      D <: Dim,
      S,
      A,
      Sem
  ](
      selected: SelectedSampled[F, D, S, A, Sem, Rank[2]]
  )
    /** Zero-copy contiguous trailing-axis values for one selected position. */
    def seriesAt(
        position: Index[selected.selection.I]
    ): NDArray[A, Rank[1]] =
      selected.data.select(0, position.ordinal)

  private def gatherValues[
      F <: Frame[D],
      D <: Dim,
      S,
      I <: SampleSpace[?, D],
      A,
      Sem,
      R <: AnyRank
  ](
      domain: GridDomain[F, D, S],
      image: Sampled[I, A, Sem, R],
      selection: Selection[S],
      shape: Shape[AnyRank]
  ): NDArray[A, AnyRank] =
    given DType[A] = image.dtype
    val spatialRank = domain.grid.shape.length
    val nonSpatialShape = image.nonSpatialAxes.shape
    val trailingSize =
      if nonSpatialShape.isEmpty then 1
      else nonSpatialShape.product
    NDArray.build[A, AnyRank](shape): output =>
      // One reusable dynamic index is sufficient for the whole gather. Grid
      // ordinals are canonical last-axis-fastest, so coordinates can be
      // decoded directly without allocating a Vector and Array per position.
      val indices = Array.ofDim[Int](spatialRank + nonSpatialShape.length)
      var position = 0
      while position < selection.size do
        val compactIndex =
          selection.positions.indexAtValidatedOrdinal(position)
        val sourceIndex = selection(compactIndex)
        var remainingSpatial = sourceIndex.ordinal
        var axis = spatialRank - 1
        while axis >= 0 do
          val extent = domain.grid.shape(axis)
          indices(axis) = remainingSpatial % extent
          remainingSpatial /= extent
          axis -= 1

        var trailingOrdinal = 0
        while trailingOrdinal < trailingSize do
          var remaining = trailingOrdinal
          axis = nonSpatialShape.length - 1
          while axis >= 0 do
            val extent = nonSpatialShape(axis)
            indices(spatialRank + axis) = remaining % extent
            remaining /= extent
            axis -= 1
          output.writeLinear(
            position * trailingSize + trailingOrdinal,
            image.data.at(IArray.unsafeFromArray(indices))
          )
          trailingOrdinal += 1
        position += 1

  private def dimensions(shape: Shape[?]): Vector[Int] =
    Vector.tabulate(shape.rank)(shape.apply)

  private def exactSelectionOwner[
      F <: Frame[D],
      D <: Dim,
      S,
      T
  ](
      domain: GridDomain[F, D, S],
      selection: Selection[T]
  ): Either[SelectedSampledError, Selection[S]] =
    if domain.space.sameRuntimeOwnerAs(selection.space) then
      // Runtime-owner equality proves that only the phantom source parameter
      // differs. Retain the exact selection object and its position owner.
      Right(selection.asInstanceOf[Selection[S]])
    else
      Left(
        SelectedSampledError.SelectionSpace(
          SpaceMismatch.between(domain.space, selection.space)
        )
      )
