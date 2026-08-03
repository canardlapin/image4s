package image4s

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import ravel.AnyRank
import ravel.Shape

/** Create-only request for a physical frame.
  *
  * A frame specification deliberately defers validation until a [[SamplingSpec]] is built. This
  * lets one composite image constructor report every construction failure through `ImageError`
  * without introducing another live frame representation.
  */
sealed trait FrameSpec derives CanEqual:
  def label: String
  def unit: LengthUnit
  def convention: CoordinateConvention

  private[image4s] def build[D <: Dim](using
      Dimension[D]
  ): Either[ImageError, Frame[D]]

object FrameSpec:
  private final case class Ephemeral(
      label: String,
      unit: LengthUnit,
      convention: CoordinateConvention
  ) extends FrameSpec:
    private[image4s] def build[D <: Dim](using
        Dimension[D]
    ): Either[ImageError, Frame[D]] =
      Frame
        .named[D](label, unit, convention)
        .left
        .map(ImageError.Geometry.apply)

  private final case class Persistent(
      id: String,
      label: String,
      unit: LengthUnit,
      convention: CoordinateConvention
  ) extends FrameSpec:
    private[image4s] def build[D <: Dim](using
        Dimension[D]
    ): Either[ImageError, Frame[D]] =
      for
        parsed <- FrameId.parse(id).left.map(ImageError.Geometry.apply)
        frame <- Frame
          .persistentNamed[D](parsed, label, unit, convention)
          .left
          .map(ImageError.Geometry.apply)
      yield frame

  /** Request a fresh ephemeral frame when the surrounding sampling specification is built. */
  def named(
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  ): FrameSpec =
    Ephemeral(label, unit, convention)

  /** Request a fresh live owner with a persistent structural frame key. */
  def persistent(
      id: String,
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  ): FrameSpec =
    Persistent(id, label, unit, convention)

/** Create-only request for the spatial index-to-frame transform of a grid.
  *
  * The grid shape is supplied later by [[SamplingSpec.buildFor]], so it cannot disagree with the
  * spatial prefix of the array shape.
  */
sealed trait GridSpec[D <: Dim]:
  private[image4s] def indexToFrame(using
      Dimension[D]
  ): Either[ImageError, Affine[D]]

object GridSpec:
  private final class Identity[D <: Dim] extends GridSpec[D]:
    private[image4s] def indexToFrame(using
        Dimension[D]
    ): Either[ImageError, Affine[D]] =
      Right(Affine.identity[D])

  private final class AxisAligned[D <: Dim](
      origin: Vector[Double],
      spacing: Vector[Double]
  ) extends GridSpec[D]:
    private[image4s] def indexToFrame(using
        dimension: Dimension[D]
    ): Either[ImageError, Affine[D]] =
      val direction =
        Vector.tabulate(dimension.rank * dimension.rank) { index =>
          if index / dimension.rank == index % dimension.rank then 1.0
          else 0.0
        }
      Affine
        .fromOriginSpacingDirection[D](origin, spacing, direction)
        .left
        .map(ImageError.Geometry.apply)

  private final class SuppliedAffine[D <: Dim](
      affine: Affine[D]
  ) extends GridSpec[D]:
    private[image4s] def indexToFrame(using
        Dimension[D]
    ): Either[ImageError, Affine[D]] =
      Right(affine)

  /** Request an explicit identity index-to-frame transform. */
  def identity[D <: Dim]: GridSpec[D] =
    new Identity[D]

  /** Request an axis-aligned transform with the declared frame origin and positive spacing. */
  def axisAligned[D <: Dim](
      origin: IterableOnce[Double],
      spacing: IterableOnce[Double]
  ): GridSpec[D] =
    new AxisAligned(origin.iterator.toVector, spacing.iterator.toVector)

  /** Reuse an already validated affine transform. */
  def affine[D <: Dim](value: Affine[D]): GridSpec[D] =
    new SuppliedAffine(value)

/** Create-only request for one non-spatial sampling axis.
  *
  * Regular and ordinal extents are bound from the corresponding trailing array extent. Explicit
  * coordinates and categorical labels carry their own extent, which must match that array extent.
  */
sealed trait AxisSpec:
  def name: String
  def kind: AxisKind

  private[image4s] def build(
      axisIndex: Int,
      boundExtent: Int
  ): Either[ImageError, Axis]

object AxisSpec:
  private final case class Ordinal(
      name: String,
      kind: AxisKind
  ) extends AxisSpec:
    private[image4s] def build(
        axisIndex: Int,
        boundExtent: Int
    ): Either[ImageError, Axis] =
      Axis.ordinal(name, kind, boundExtent)

  private final case class Regular(
      name: String,
      kind: AxisKind,
      origin: Double,
      step: Double,
      unit: AxisUnit
  ) extends AxisSpec:
    private[image4s] def build(
        axisIndex: Int,
        boundExtent: Int
    ): Either[ImageError, Axis] =
      Axis.regular(name, kind, boundExtent, origin, step, unit)

  private final case class Explicit(
      name: String,
      kind: AxisKind,
      values: Vector[Double],
      unit: AxisUnit
  ) extends AxisSpec:
    private[image4s] def build(
        axisIndex: Int,
        boundExtent: Int
    ): Either[ImageError, Axis] =
      validateExtent(axisIndex, name, values.size, boundExtent)
        .flatMap(_ => Axis.explicit(name, kind, values, unit))

  private final case class Categorical(
      name: String,
      kind: AxisKind,
      labels: Vector[String]
  ) extends AxisSpec:
    private[image4s] def build(
        axisIndex: Int,
        boundExtent: Int
    ): Either[ImageError, Axis] =
      validateExtent(axisIndex, name, labels.size, boundExtent)
        .flatMap(_ => Axis.categorical(name, kind, labels))

  def ordinal(name: String, kind: AxisKind): AxisSpec =
    Ordinal(name, kind)

  def regular(
      name: String,
      kind: AxisKind,
      origin: Double,
      step: Double,
      unit: AxisUnit
  ): AxisSpec =
    Regular(name, kind, origin, step, unit)

  def explicit(
      name: String,
      kind: AxisKind,
      values: IterableOnce[Double],
      unit: AxisUnit
  ): AxisSpec =
    Explicit(name, kind, values.iterator.toVector, unit)

  def categorical(
      name: String,
      kind: AxisKind,
      labels: IterableOnce[String]
  ): AxisSpec =
    Categorical(name, kind, labels.iterator.toVector)

  /** Concise request for the common single regular time axis. */
  def timeRegular(
      origin: Double,
      step: Double,
      unit: AxisUnit,
      name: String = "time"
  ): AxisSpec =
    regular(name, AxisKind.Time, origin, step, unit)

  private def validateExtent(
      axisIndex: Int,
      name: String,
      declared: Int,
      bound: Int
  ): Either[ImageError, Unit] =
    Either.cond(
      declared == bound,
      (),
      ImageError.AxisSpecificationExtentMismatch(
        axisIndex,
        name,
        declared,
        bound
      )
    )

/** Ordered create-only requests for the non-spatial axes of a sampled image. */
final class AxesSpec private[image4s] (
    val values: Vector[AxisSpec]
):
  def size: Int =
    values.size

  private[image4s] def build(
      extents: Vector[Int]
  ): Either[ImageError, NonSpatialAxes] =
    values
      .zip(extents)
      .zipWithIndex
      .foldLeft[Either[ImageError, Vector[Axis]]](Right(Vector.empty)) {
        case (result, ((specification, extent), axisIndex)) =>
          for
            built <- result
            axis <- specification.build(axisIndex, extent)
          yield built :+ axis
      }
      .flatMap(NonSpatialAxes.from)

object AxesSpec:
  val empty: AxesSpec =
    new AxesSpec(Vector.empty)

  def apply(axes: AxisSpec*): AxesSpec =
    from(axes)

  def from(axes: IterableOnce[AxisSpec]): AxesSpec =
    new AxesSpec(axes.iterator.toVector)

/** Declarative request for one complete sampling space of spatial dimension `D`.
  *
  * Building a specification constructs the ordinary [[image4s.geometry.Frame]],
  * [[image4s.geometry.Grid]], [[Axis]], and [[SampleSpace]] values. The specification is not
  * retained by the result and never owns image storage.
  */
final case class SamplingSpec[D <: Dim](
    frame: FrameSpec,
    grid: GridSpec[D],
    axes: AxesSpec = AxesSpec.empty
):
  /** Bind this request to a Ravel shape.
    *
    * The first `D` extents become the grid shape. Remaining extents bind, in order, to the declared
    * non-spatial axes.
    */
  def buildFor[R <: AnyRank](
      shape: Shape[R]
  )(using
      dimension: Dimension[D]
  ): Either[
    ImageError,
    SampleSpace[? <: Frame[D], D]
  ] =
    buildFor(Vector.tabulate(shape.rank)(shape.apply))

  /** Bind this request to an explicit logical shape. */
  def buildFor(
      shape: IterableOnce[Int]
  )(using
      dimension: Dimension[D]
  ): Either[
    ImageError,
    SampleSpace[? <: Frame[D], D]
  ] =
    val copied = shape.iterator.toVector
    val expectedRank = dimension.rank + axes.size
    if copied.size != expectedRank then
      Left(
        ImageError.SamplingSpecificationRankMismatch(
          dimension.rank,
          axes.size,
          copied.size
        )
      )
    else
      for
        builtFrame <- frame.build[D]
        transform <- grid.indexToFrame
        builtGrid <- Grid
          .forFrame(builtFrame)(copied.take(dimension.rank), transform)
          .left
          .map(ImageError.Geometry.apply)
        builtAxes <- axes.build(copied.drop(dimension.rank))
      yield SampleSpace.create(builtGrid, builtAxes)
