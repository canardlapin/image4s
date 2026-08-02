package image4s.morphology

import image4s.Continuous
import image4s.Mask
import image4s.MaskImage
import image4s.SampleSpace
import image4s.Sampled
import image4s.ValueSemantics
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import image4s.ops.Border
import image4s.ops.Offset
import image4s.ops.OpError
import image4s.ops.Radius
import image4s.ops.Support
import ravel.AnyRank
import ravel.DType.given
import ravel.MutableNDArray
import ravel.NDArray
import ravel.OrderedDType
import ravel.*
import ravel.map
import ravel.stencil.BooleanNeighborhoodReducer
import ravel.stencil.BorderMode
import ravel.stencil.DirectNeighborhoodExecutor
import ravel.stencil.NeighborhoodSpec

/** Scalar comparison used when deriving a logical mask. */
enum ThresholdComparison derives CanEqual:
  case GreaterThan, GreaterOrEqual, LessThan, LessOrEqual

  private[morphology] def matches[A](
      ordering: Ordering[A],
      value: A,
      threshold: A
  ): Boolean =
    val comparison = ordering.compare(value, threshold)
    this match
      case GreaterThan    => comparison > 0
      case GreaterOrEqual => comparison >= 0
      case LessThan       => comparison < 0
      case LessOrEqual    => comparison <= 0

private object ThresholdOrdering:
  /** Ravel's ordered kernels implement the same compare relation as these
    * canonical Scala orderings. Other orderings must retain the callback path.
    */
  def isCanonical[A](ordering: Ordering[A]): Boolean =
    val ref = ordering.asInstanceOf[AnyRef]
    (ref eq Ordering.Byte) ||
      (ref eq Ordering.Short) ||
      (ref eq Ordering.Int) ||
      (ref eq Ordering.Long) ||
      (ref eq Ordering.Float.TotalOrdering) ||
      (ref eq Ordering.Float.IeeeOrdering) ||
      (ref eq Ordering.DeprecatedFloatOrdering) ||
      (ref eq Ordering.Double.TotalOrdering) ||
      (ref eq Ordering.Double.IeeeOrdering) ||
      (ref eq Ordering.DeprecatedDoubleOrdering)

private def primitiveThresholdMask[A, R <: AnyRank](
    data: NDArray[A, R],
    value: A,
    comparison: ThresholdComparison
)(using ordered: OrderedDType[A]): NDArray[Boolean, R] =
  val result =
    comparison match
      case ThresholdComparison.GreaterThan => data.orderedGreaterThan(value)
      case ThresholdComparison.GreaterOrEqual =>
        data.orderedGreaterOrEqual(value)
      case ThresholdComparison.LessThan => data.orderedLessThan(value)
      case ThresholdComparison.LessOrEqual => data.orderedLessOrEqual(value)
  result.asInstanceOf[NDArray[Boolean, R]]

/** Geometry of a flat binary structuring element. */
enum StructuringElementShape derives CanEqual:
  case Box, Cross, Disk, Ball

/** Deferred structuring-element definition.
  *
  * Frame-space radii are lowered against a grid at preparation time, so disks
  * and balls use the affine-induced physical metric rather than treating
  * samples as isotropic.
  */
final class StructuringElement[D <: Dim] private (
    val shape: StructuringElementShape,
    val radius: Radius
):
  private[morphology] def support[F <: Frame[D]](
      grid: Grid[F, D]
  )(using dimension: Dimension[D]): Either[OpError, Support[D]] =
    val rank = dimension.rank
    shape match
      case StructuringElementShape.Disk if rank != 2 =>
        Left(OpError.InvalidArgument("Disk structuring elements require D2"))
      case StructuringElementShape.Ball if rank != 3 =>
        Left(OpError.InvalidArgument("Ball structuring elements require D3"))
      case _ =>
        for
          geometry <- RadiusGeometry.from(grid, radius)
          offsets <- StructuringElement.offsets(shape, geometry)
          support <- Support.create(offsets.map(Offset.unsafe[D]))
        yield support

object StructuringElement:
  def box[D <: Dim](radius: Radius): StructuringElement[D] =
    new StructuringElement(StructuringElementShape.Box, radius)

  def cross[D <: Dim](radius: Radius): StructuringElement[D] =
    new StructuringElement(StructuringElementShape.Cross, radius)

  def disk[D <: Dim](radius: Radius): StructuringElement[D] =
    new StructuringElement(StructuringElementShape.Disk, radius)

  def ball[D <: Dim](radius: Radius): StructuringElement[D] =
    new StructuringElement(StructuringElementShape.Ball, radius)

  private def offsets(
      shape: StructuringElementShape,
      geometry: RadiusGeometry
  ): Either[OpError, Vector[Vector[Int]]] =
    val candidates =
      cartesian(
        Vector.fill(geometry.rank)(
          (-geometry.bound to geometry.bound).toVector
        )
      )
    Right(candidates.filter(geometry.includes(shape, _)))

  private def cartesian(ranges: Vector[Vector[Int]]): Vector[Vector[Int]] =
    ranges.foldLeft(Vector(Vector.empty[Int])) { (prefixes, range) =>
      for
        prefix <- prefixes
        coordinate <- range
      yield prefix :+ coordinate
    }

private final class RadiusGeometry(
    val rank: Int,
    val bound: Int,
    val includes: (StructuringElementShape, Vector[Int]) => Boolean
)

private object RadiusGeometry:
  def from[D <: Dim, F <: Frame[D]](
      grid: Grid[F, D],
      radius: Radius
  )(using dimension: Dimension[D]): Either[OpError, RadiusGeometry] =
    radius match
      case samples: Radius.Samples =>
        Right(
          new RadiusGeometry(
            dimension.rank,
            samples.value,
            (shape, coordinates) => includesSamples(shape, samples.value, coordinates)
          )
        )
      case frame: Radius.FrameUnits =>
        fromFrame(grid, frame)

  private def fromFrame[D <: Dim, F <: Frame[D]](
      grid: Grid[F, D],
      radius: Radius.FrameUnits
  )(using dimension: Dimension[D]): Either[OpError, RadiusGeometry] =
    val rank = dimension.rank
    val physicalRadius =
      radius.value * unitScale(radius.unit.getOrElse(grid.frame.unit)) /
        unitScale(grid.frame.unit)
    val matrix = grid.indexToFrame.rowMajor
    val columns =
      Vector.tabulate(rank) { column =>
        Vector.tabulate(rank) { row =>
          matrix(row * (rank + 1) + column)
        }
      }
    val norms =
      columns.map(column => math.sqrt(column.map(value => value * value).sum))
    if norms.exists(value => !value.isFinite || value <= 0.0) then
      Left(OpError.InvalidScale("grid has a degenerate spatial axis"))
    else
      val bound = math.ceil(physicalRadius / norms.min).toInt
      Right(
        new RadiusGeometry(
          rank,
          bound,
          (shape, coordinates) =>
            shape match
              case StructuringElementShape.Box =>
                coordinates.indices.forall { axis =>
                  coordinates(axis).abs.toDouble * norms(axis) <= physicalRadius
                }
              case StructuringElementShape.Cross =>
                coordinates.count(_ != 0) <= 1 &&
                  frameDistanceSquared(columns, coordinates) <=
                    physicalRadius * physicalRadius
              case StructuringElementShape.Disk | StructuringElementShape.Ball =>
                frameDistanceSquared(columns, coordinates) <=
                  physicalRadius * physicalRadius
        )
      )

  private def includesSamples(
      shape: StructuringElementShape,
      radius: Int,
      coordinates: Vector[Int]
  ): Boolean =
    shape match
      case StructuringElementShape.Box =>
        coordinates.forall(_.abs <= radius)
      case StructuringElementShape.Cross =>
        coordinates.count(_ != 0) <= 1 && coordinates.forall(_.abs <= radius)
      case StructuringElementShape.Disk | StructuringElementShape.Ball =>
        coordinates.foldLeft(0L) { (sum, coordinate) =>
          sum + coordinate.toLong * coordinate.toLong
        } <= radius.toLong * radius.toLong

  private def frameDistanceSquared(
      columns: Vector[Vector[Double]],
      coordinates: Vector[Int]
  ): Double =
    var sum = 0.0
    var row = 0
    while row < columns.length do
      var coordinate = 0.0
      var axis = 0
      while axis < columns.length do
        coordinate += columns(axis)(row) * coordinates(axis).toDouble
        axis += 1
      sum += coordinate * coordinate
      row += 1
    sum

  private def unitScale(unit: LengthUnit): Double =
    unit match
      case LengthUnit.Meter      => 1.0
      case LengthUnit.Millimeter => 1.0e-3
      case LengthUnit.Micrometer => 1.0e-6

private enum BinaryOperation:
  case Erode, Dilate

private trait PreparedBooleanPass[R <: AnyRank]:
  def run(
      source: NDArray[Boolean, R],
      destination: MutableNDArray[Boolean, R]
  ): Unit

  def runMutable(
      source: MutableNDArray[Boolean, R],
      destination: MutableNDArray[Boolean, R]
  ): Unit

private object PreparedBooleanPass:
  def prepare[R <: AnyRank](
      source: NDArray[Boolean, R],
      destination: MutableNDArray[Boolean, R],
      spec: NeighborhoodSpec,
      operation: BinaryOperation,
      constant: Boolean
  ): PreparedBooleanPass[R] =
    val reducer = reducerFor(operation)
    val prepared =
      DirectNeighborhoodExecutor.prepare(source, destination, spec)
    new PreparedBooleanPass[R]:
      def run(
          nextSource: NDArray[Boolean, R],
          nextDestination: MutableNDArray[Boolean, R]
      ): Unit =
        prepared.runBoolean(nextSource, nextDestination, reducer, constant)

      def runMutable(
          nextSource: MutableNDArray[Boolean, R],
          nextDestination: MutableNDArray[Boolean, R]
      ): Unit =
        prepared.runBoolean(nextSource, nextDestination, reducer, constant)

  def prepareMutable[R <: AnyRank](
      source: MutableNDArray[Boolean, R],
      destination: MutableNDArray[Boolean, R],
      spec: NeighborhoodSpec,
      operation: BinaryOperation,
      constant: Boolean
  ): PreparedBooleanPass[R] =
    val reducer = reducerFor(operation)
    val prepared =
      DirectNeighborhoodExecutor.prepare(source, destination, spec)
    new PreparedBooleanPass[R]:
      def run(
          nextSource: NDArray[Boolean, R],
          nextDestination: MutableNDArray[Boolean, R]
      ): Unit =
        prepared.runBoolean(nextSource, nextDestination, reducer, constant)

      def runMutable(
          nextSource: MutableNDArray[Boolean, R],
          nextDestination: MutableNDArray[Boolean, R]
      ): Unit =
        prepared.runBoolean(nextSource, nextDestination, reducer, constant)

  private def reducerFor(operation: BinaryOperation): BooleanNeighborhoodReducer =
    operation match
      case BinaryOperation.Erode =>
        new BooleanNeighborhoodReducer:
          def zero: Boolean = true
          def accumulate(
              accumulator: Boolean,
              value: Boolean,
              offsetIndex: Int
          ): Boolean =
            accumulator && value

          override def isTerminal(accumulator: Boolean): Boolean =
            !accumulator

          def finish(accumulator: Boolean): Boolean = accumulator
      case BinaryOperation.Dilate =>
        new BooleanNeighborhoodReducer:
          def zero: Boolean = false
          def accumulate(
              accumulator: Boolean,
              value: Boolean,
              offsetIndex: Int
          ): Boolean =
            accumulator || value

          override def isTerminal(accumulator: Boolean): Boolean =
            accumulator

          def finish(accumulator: Boolean): Boolean = accumulator

/** Prepared binary morphology schedule with support and address plan reuse.
  *
  * A plan owns mutable Boolean workspaces and is sequential. Every `run`
  * returns a fresh immutable mask, so results do not alias later runs.
  */
final class PreparedBinaryMorphology[
    D <: Dim,
    F <: Frame[D],
    S <: SampleSpace[?, ?],
    R <: AnyRank
] private[morphology] (
    expectedSpace: S,
    workspace: MutableNDArray[Boolean, R],
    alternateWorkspace: Option[MutableNDArray[Boolean, R]],
    first: PreparedBooleanPass[R],
    remaining: Vector[PreparedBooleanPass[R]],
    val support: Support[D]
)(using semantics: ValueSemantics[Boolean, Mask]):
  def run[S2 <: SampleSpace[?, ?]](
      input: MaskImage[S2, R]
  ): Either[OpError, MaskImage[? <: SampleSpace[F, D], R]] =
    if !(input.sampleSpace eq expectedSpace) then
      Left(
        OpError.InvalidArgument(
          "prepared morphology requires the same live sample-space owner"
        )
      )
    else
      first.run(input.data, workspace)
      val result =
        alternateWorkspace match
          case None => workspace
          case Some(alternate) =>
            remaining.foreach { pass =>
              pass.runMutable(workspace, alternate)
            }
            alternate
      Sampled
        .mask(
          input.sampleSpace.asInstanceOf[SampleSpace[F, D]],
          result.freezeCopy(),
          input.metadata
        )
        .left
        .map(OpError.Image.apply)

object BinaryMorphology:
  def prepareErode[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      border: Border[Boolean] = Border.Constant(true)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[
    OpError,
    PreparedBinaryMorphology[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      R
    ]
  ] =
    prepare(input, element, Vector(BinaryOperation.Erode), Vector(border))

  def prepareDilate[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      border: Border[Boolean] = Border.Constant(false)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[
    OpError,
    PreparedBinaryMorphology[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      R
    ]
  ] =
    prepare(input, element, Vector(BinaryOperation.Dilate), Vector(border))

  def prepareOpen[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      erosionBorder: Border[Boolean] = Border.Constant(true),
      dilationBorder: Border[Boolean] = Border.Constant(false)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[
    OpError,
    PreparedBinaryMorphology[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      R
    ]
  ] =
    prepare(
      input,
      element,
      Vector(BinaryOperation.Erode, BinaryOperation.Dilate),
      Vector(erosionBorder, dilationBorder)
    )

  def prepareClose[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      dilationBorder: Border[Boolean] = Border.Constant(false),
      erosionBorder: Border[Boolean] = Border.Constant(true)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[
    OpError,
    PreparedBinaryMorphology[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      R
    ]
  ] =
    prepare(
      input,
      element,
      Vector(BinaryOperation.Dilate, BinaryOperation.Erode),
      Vector(dilationBorder, erosionBorder)
    )

  def erode[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      border: Border[Boolean] = Border.Constant(true)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    prepareErode(input, element, border).flatMap(_.run(input))

  def dilate[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      border: Border[Boolean] = Border.Constant(false)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    prepareDilate(input, element, border).flatMap(_.run(input))

  def open[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      erosionBorder: Border[Boolean] = Border.Constant(true),
      dilationBorder: Border[Boolean] = Border.Constant(false)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    prepareOpen(input, element, erosionBorder, dilationBorder).flatMap(_.run(input))

  def close[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      dilationBorder: Border[Boolean] = Border.Constant(false),
      erosionBorder: Border[Boolean] = Border.Constant(true)
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    prepareClose(input, element, dilationBorder, erosionBorder).flatMap(_.run(input))

  private def prepare[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      input: MaskImage[S, R],
      element: StructuringElement[input.sampleSpace.D],
      operations: Vector[BinaryOperation],
      borders: Vector[Border[Boolean]]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[
    OpError,
    PreparedBinaryMorphology[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      R
    ]
  ] =
    element.support(input.grid).map { support =>
      val shape = input.data.shape
      val spec =
        NeighborhoodSpec(
          spatialAxes = dimension.rank,
          offsets = support.offsets.map(_.coordinates),
          border = borderConfiguration(borders.head)._1,
          outputOrigin = Vector.fill(dimension.rank)(0),
          outputSpatialShape = input.grid.shape
        )
      val workspace = MutableNDArray.zeros[Boolean, R](shape)
      val first =
        PreparedBooleanPass.prepare(
          input.data,
          workspace,
          spec,
          operations.head,
          borderConfiguration(borders.head)._2
        )
      val alternate =
        if operations.length == 1 then None
        else Some(MutableNDArray.zeros[Boolean, R](shape))
      val remaining =
        alternate.toVector.flatMap { destination =>
          operations.drop(1).zip(borders.drop(1)).map { case (operation, border) =>
            val configured = borderConfiguration(border)
            PreparedBooleanPass.prepareMutable(
              workspace,
              destination,
              spec.copy(border = configured._1),
              operation,
              configured._2
            )
          }
        }
      new PreparedBinaryMorphology[
        input.sampleSpace.D,
        input.sampleSpace.F,
        S,
        R
      ](
        input.sampleSpace,
        workspace,
        alternate,
        first,
        remaining,
        support
      )
    }

  private def borderConfiguration(
      border: Border[Boolean]
  ): (BorderMode, Boolean) =
    border match
      case Border.Constant(value)      => (BorderMode.Constant, value)
      case Border.Replicate            => (BorderMode.Replicate, false)
      case Border.ReflectWithoutEdge   => (BorderMode.ReflectWithoutEdge, false)
      case Border.ReflectWithEdge      => (BorderMode.ReflectWithEdge, false)
      case Border.Wrap                 => (BorderMode.Wrap, false)

extension [
    S <: SampleSpace[?, ?],
    A,
    R <: AnyRank
](input: Sampled[S, A, Continuous, R])
  def threshold(
      value: A,
      comparison: ThresholdComparison = ThresholdComparison.GreaterOrEqual
  )(using
      ordering: Ordering[A],
      semantics: ValueSemantics[Boolean, Mask]
  ): Either[
    OpError,
    MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]
  ] =
    val mask =
      if ThresholdOrdering.isCanonical(ordering) &&
        input.data.dtype.isInstanceOf[OrderedDType[?]]
      then
        primitiveThresholdMask(
          input.data,
          value,
          comparison
        )(using input.data.dtype.asInstanceOf[OrderedDType[A]])
      else
        input.data.map(sample => comparison.matches(ordering, sample, value))
    Sampled
      .mask(input.sampleSpace, mask, input.metadata)
      .left
      .map(OpError.Image.apply)

extension [
    S <: SampleSpace[?, ?],
    R <: AnyRank
](input: MaskImage[S, R])
  def erode(
      element: StructuringElement[input.sampleSpace.D],
      border: Border[Boolean] = Border.Constant(true)
  )(using
      Dimension[input.sampleSpace.D],
      ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    BinaryMorphology.erode(input, element, border)

  def dilate(
      element: StructuringElement[input.sampleSpace.D],
      border: Border[Boolean] = Border.Constant(false)
  )(using
      Dimension[input.sampleSpace.D],
      ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    BinaryMorphology.dilate(input, element, border)

  def open(
      element: StructuringElement[input.sampleSpace.D],
      erosionBorder: Border[Boolean] = Border.Constant(true),
      dilationBorder: Border[Boolean] = Border.Constant(false)
  )(using
      Dimension[input.sampleSpace.D],
      ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    BinaryMorphology.open(input, element, erosionBorder, dilationBorder)

  def close(
      element: StructuringElement[input.sampleSpace.D],
      dilationBorder: Border[Boolean] = Border.Constant(false),
      erosionBorder: Border[Boolean] = Border.Constant(true)
  )(using
      Dimension[input.sampleSpace.D],
      ValueSemantics[Boolean, Mask]
  ): Either[OpError, MaskImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], R]] =
    BinaryMorphology.close(input, element, dilationBorder, erosionBorder)
