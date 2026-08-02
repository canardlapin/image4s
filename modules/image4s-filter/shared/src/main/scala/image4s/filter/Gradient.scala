package image4s.filter

import image4s.Continuous
import image4s.ContinuousImage
import image4s.SampleSpace
import image4s.Sampled
import image4s.ValueSemantics
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.ops.AxisKernel
import image4s.ops.Border
import image4s.ops.CoordinateDomain
import image4s.ops.Correlation
import image4s.ops.FilterExtent
import image4s.ops.FrameCoordinates
import image4s.ops.IndexCoordinates
import image4s.ops.Kernel
import image4s.ops.OpError
import ravel.AnyRank
import ravel.FloatingDType
import ravel.NDArray
import ravel.NumericDType

/** Discrete gradient stencil family. Both variants are normalized so a
  * unit-slope index-space affine field has an interior derivative of one.
  */
enum GradientOperator derives CanEqual:
  case Sobel, Scharr

/** Component images of a gradient expressed in one explicit coordinate domain.
  *
  * Components are in grid-axis order. `FrameCoordinates` applies the
  * `indexToFrame^{-T}` transform to the sampled index gradient, never merely a
  * spacing division.
  */
final class GradientField[
    D <: Dim,
    F <: Frame[D],
    A,
    R <: AnyRank
] private[filter] (
    val domain: CoordinateDomain,
    val components: Vector[ContinuousImage[? <: SampleSpace[F, D], A, R]]
):
  def component(axis: Int): Option[ContinuousImage[? <: SampleSpace[F, D], A, R]] =
    components.lift(axis)

object Gradient:
  def sobel[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[A] = Border.reflect
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, A, R]
  ] =
    gradient(input, GradientOperator.Sobel, domain, border)

  def scharr[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[A] = Border.reflect
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, A, R]
  ] =
    gradient(input, GradientOperator.Scharr, domain, border)

  def sobelTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[B] = Border.reflect
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, B, R]
  ] =
    gradientTo(input, GradientOperator.Sobel, domain, border)

  def scharrTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[B] = Border.reflect
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, B, R]
  ] =
    gradientTo(input, GradientOperator.Scharr, domain, border)

  private def gradient[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operator: GradientOperator,
      domain: CoordinateDomain,
      border: Border[A]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, A, R]
  ] =
    filterComponents(input, operator, border).flatMap { components =>
      inDomain(input, domain, components)
    }

  private def gradientTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operator: GradientOperator,
      domain: CoordinateDomain,
      border: Border[B]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, B, R]
  ] =
    filterComponentsTo(input, operator, border).flatMap { components =>
      inDomain(input, domain, components)
    }

  private def filterComponents[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operator: GradientOperator,
      border: Border[A]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    Vector[ContinuousImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], A, R]]
  ] =
    Vector
      .tabulate(dimension.rank) { axis =>
        for
          kernel <- kernelFor[input.sampleSpace.D, A](operator, axis)
          component <- LinearFilter.correlate(
            input,
            Correlation(kernel, FilterExtent.same(border))
          )
        yield component
      }
      .foldLeft(Right(Vector.empty): Either[
        OpError,
        Vector[
          ContinuousImage[
            ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
            A,
            R
          ]
        ]
      ]) { (accumulated, next) =>
        for
          values <- accumulated
          component <- next
        yield values :+ component
      }

  private def filterComponentsTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operator: GradientOperator,
      border: Border[B]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    Vector[ContinuousImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], B, R]]
  ] =
    Vector
      .tabulate(dimension.rank) { axis =>
        for
          kernel <- kernelFor[input.sampleSpace.D, B](operator, axis)
          component <- LinearFilter.correlateTo(
            input,
            Correlation(kernel, FilterExtent.same(border))
          )
        yield component
      }
      .foldLeft(Right(Vector.empty): Either[
        OpError,
        Vector[
          ContinuousImage[
            ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
            B,
            R
          ]
        ]
      ]) { (accumulated, next) =>
        for
          values <- accumulated
          component <- next
        yield values :+ component
      }

  private def kernelFor[D <: Dim, A](
      operator: GradientOperator,
      derivativeAxis: Int
  )(using
      dimension: Dimension[D],
      target: FilterOutput[A]
  ): Either[OpError, Kernel.Separable[D, A]] =
    val derivative =
      AxisKernel.centered(
        Vector(-0.5, 0.0, 0.5).map(target.fromDouble)
      )
    val smoothing =
      operator match
        case GradientOperator.Sobel =>
          AxisKernel.centered(
            Vector(0.25, 0.5, 0.25).map(target.fromDouble)
          )
        case GradientOperator.Scharr =>
          AxisKernel.centered(
            Vector(3.0 / 16.0, 10.0 / 16.0, 3.0 / 16.0).map(
              target.fromDouble
            )
          )
    for
      derivativeFactor <- derivative
      smoothingFactor <- smoothing
      kernel <- Kernel.separable(
        Vector.tabulate(dimension.rank) { axis =>
          if axis == derivativeAxis then derivativeFactor
          else smoothingFactor
        }
      )
    yield kernel

  private def inDomain[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      domain: CoordinateDomain,
      components: Vector[
        ContinuousImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], B, R]
      ]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, B, R]
  ] =
    domain match
      case IndexCoordinates =>
        Right(new GradientField(domain, components))
      case FrameCoordinates =>
        transformToFrame(input, components).map(new GradientField(domain, _))

  private def transformToFrame[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      components: Vector[
        ContinuousImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], B, R]
      ]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    Vector[ContinuousImage[? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D], B, R]]
  ] =
    given ravel.DType[B] = floating
    val rank = dimension.rank
    val inverse = input.grid.indexToFrame.inverse.rowMajor
    val sources = components.map(_.data.elementsIterator.toVector)
    val shape = components.head.data.shape
    val size = components.head.data.size
    Vector
      .tabulate(rank) { frameAxis =>
        val values =
          Vector.tabulate(size) { linear =>
            var indexAxis = 0
            var value = 0.0
            while indexAxis < rank do
              value +=
                inverse(indexAxis * (rank + 1) + frameAxis) *
                  target.toDouble(sources(indexAxis)(linear))
              indexAxis += 1
            target.fromDouble(value)
          }
        Sampled
          .continuous(
            components.head.sampleSpace,
            NDArray.fromSeq(shape, values),
            input.metadata
          )
          .left
          .map(OpError.Image.apply)
      }
      .foldLeft(Right(Vector.empty): Either[
        OpError,
        Vector[
          ContinuousImage[
            ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
            B,
            R
          ]
        ]
      ]) { (accumulated, next) =>
        for
          values <- accumulated
          component <- next
        yield values :+ component
      }

extension [
    S <: SampleSpace[?, ?],
    A,
    R <: AnyRank
](input: Sampled[S, A, Continuous, R])
  def gradient(
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[A] = Border.reflect
  )(using
      Dimension[input.sampleSpace.D],
      FilterOutput[A],
      FloatingDType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, A, R]
  ] =
    Gradient.sobel(input, domain, border)

  def gradientTo[B](
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[B] = Border.reflect
  )(using
      Dimension[input.sampleSpace.D],
      NumericDType[A],
      FilterOutput[B],
      FloatingDType[B],
      ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, B, R]
  ] =
    Gradient.sobelTo(input, domain, border)

  def sobel(
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[A] = Border.reflect
  )(using
      Dimension[input.sampleSpace.D],
      FilterOutput[A],
      FloatingDType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, A, R]
  ] =
    Gradient.sobel(input, domain, border)

  def scharr(
      domain: CoordinateDomain = IndexCoordinates,
      border: Border[A] = Border.reflect
  )(using
      Dimension[input.sampleSpace.D],
      FilterOutput[A],
      FloatingDType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    GradientField[input.sampleSpace.D, input.sampleSpace.F, A, R]
  ] =
    Gradient.scharr(input, domain, border)
