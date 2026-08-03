package image4s

import ravel.AnyRank
import ravel.CanDropAxis
import ravel.DType
import ravel.DropAxis
import ravel.NDArray
import ravel.Rank
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid

/** A sampled image whose spatial dimension, frame owner, and storage rank were discovered at
  * runtime.
  *
  * `SomeSampled` contains the original [[Sampled]] value. It does not copy its data or define
  * another image representation. Use `fold` to recover either a D2 or D3 case while retaining the
  * hidden frame and rank as path-dependent type members.
  */
sealed trait SomeSampled[A, Sem]:
  type D <: Dim
  type F <: Frame[D]
  type S <: SampleSpace[F, D]
  type R <: AnyRank

  val dimension: Dimension[D]
  val value: Sampled[S, A, Sem, R]

  private[image4s] val dropAxisEvidence: CanDropAxis[R]

  final def sampleSpace: S =
    value.sampleSpace

  final def grid: Grid[F, D] =
    value.grid

  final def data: NDArray[A, R] =
    value.data

  final def dtype: DType[A] =
    value.dtype

  final def metadata: ImageMetadata =
    value.metadata

  final def logicalShape: Vector[Int] =
    value.logicalShape

  final def nonSpatialAxes: NonSpatialAxes =
    value.nonSpatialAxes

  final def spatialRank: Int =
    dimension.rank

  final def storageRank: Int =
    value.data.shape.rank

  final def withMetadata(next: ImageMetadata): SomeSampled[A, Sem] =
    repack(value.withMetadata(next))

  /** Map values while preserving their element type, semantic role, hidden owners, and rank. */
  final def mapValues(
      transform: A => A
  ): SomeSampled[A, Sem] =
    repack(value.mapValues(transform)(using value.dtype))

  /** Map values while selecting the output semantic role through the same evidence as Sampled. */
  final def mapValuesAs[B, OutSem](
      transform: A => B
  )(using
      ValueSemantics[B, OutSem],
      DType[B]
  ): SomeSampled[B, OutSem] =
    repack(value.mapValuesAs[B, OutSem](transform))

  final def valueAt(
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[ImageError, A] =
    value.valueAt(spatialIndex, nonSpatialIndex)

  /** Refine the hidden storage rank without copying or folding on spatial dimension. */
  final def requireDataRank[N <: Int](using
      ValueOf[N]
  ): Either[ImageError, Sampled[S, A, Sem, Rank[N]]] =
    value.requireDataRank[N]

  /** Fix one non-spatial coordinate using the rank evidence captured by this package.
    *
    * The result is the precise rank-lowered Sampled rather than a repackaged dynamic value. This
    * preserves `DropAxis[R]` without pretending that image4s can derive evidence for dropping that
    * new rank again.
    */
  final def selectNonSpatial(
      axis: Int,
      index: Int
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, DropAxis[R]]
  ] =
    given CanDropAxis[R] = dropAxisEvidence
    value.selectNonSpatial(axis, index)

  final def selectAxis(
      kind: AxisKind,
      index: Int
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, DropAxis[R]]
  ] =
    given CanDropAxis[R] = dropAxisEvidence
    value.selectAxis(kind, index)

  final def selectTime(
      index: Int
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, DropAxis[R]]
  ] =
    given CanDropAxis[R] = dropAxisEvidence
    value.selectTime(index)

  /** Approachable spelling for selecting the sole declared time axis. */
  final def atTime(
      index: Int
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, DropAxis[R]]
  ] =
    selectTime(index)

  final def selectChannel(
      index: Int
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, DropAxis[R]]
  ] =
    given CanDropAxis[R] = dropAxisEvidence
    value.selectChannel(index)

  final def selectDirection(
      index: Int
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[F, D], A, Sem, DropAxis[R]]
  ] =
    given CanDropAxis[R] = dropAxisEvidence
    value.selectDirection(index)

  /** Apply an affine-correct zero-copy crop while keeping this value dynamically packaged. */
  final def crop(
      origin: Vector[Int],
      shape: Vector[Int]
  ): Either[ImageError, SomeSampled[A, Sem]] =
    value.crop(origin, shape)(using dimension).map(repack)

  final def flipSpatial(
      axis: Int
  ): Either[ImageError, SomeSampled[A, Sem]] =
    value.flipSpatial(axis)(using dimension).map(repack)

  final def permuteSpatial(
      sourceAxisForTarget: IterableOnce[Int]
  ): Either[ImageError, SomeSampled[A, Sem]] =
    value
      .permuteSpatial(sourceAxisForTarget)(using dimension)
      .map(repack)

  final def strideSpatial(
      steps: IterableOnce[Int]
  ): Either[ImageError, SomeSampled[A, Sem]] =
    value.strideSpatial(steps)(using dimension).map(repack)

  final def permuteNonSpatial(
      order: IterableOnce[Int]
  ): Either[ImageError, SomeSampled[A, Sem]] =
    value.permuteNonSpatial(order).map(repack)

  final def canonicalLayout: SomeSampled[A, Sem] =
    repack(value.canonicalLayout)

  final def materializedCopy: SomeSampled[A, Sem] =
    repack(value.materializedCopy)

  def fold[B](
      onD2: SomeSampled.D2Case[A, Sem] => B,
      onD3: SomeSampled.D3Case[A, Sem] => B
  ): B

  private[image4s] def repack[
      B,
      OutSem,
      S2 <: SampleSpace[F, D]
  ](
      sampled: Sampled[S2, B, OutSem, R]
  ): SomeSampled[B, OutSem]

object SomeSampled:
  sealed trait D2Case[A, Sem] extends SomeSampled[A, Sem]:
    final type D = D2

    final def fold[B](
        onD2: D2Case[A, Sem] => B,
        onD3: D3Case[A, Sem] => B
    ): B =
      onD2(this)

    private[image4s] final def repack[
        B,
        OutSem,
        S2 <: SampleSpace[F, D2]
    ](
        sampled: Sampled[S2, B, OutSem, R]
    ): SomeSampled[B, OutSem] =
      SomeSampled.d2(sampled)(using dropAxisEvidence)

  sealed trait D3Case[A, Sem] extends SomeSampled[A, Sem]:
    final type D = D3

    final def fold[B](
        onD2: D2Case[A, Sem] => B,
        onD3: D3Case[A, Sem] => B
    ): B =
      onD3(this)

    private[image4s] final def repack[
        B,
        OutSem,
        S2 <: SampleSpace[F, D3]
    ](
        sampled: Sampled[S2, B, OutSem, R]
    ): SomeSampled[B, OutSem] =
      SomeSampled.d3(sampled)(using dropAxisEvidence)

  def d2[
      A,
      Sem,
      F0 <: Frame[D2],
      S0 <: SampleSpace[F0, D2],
      R0 <: AnyRank
  ](
      sampled: Sampled[S0, A, Sem, R0]
  )(using
      dropAxis: CanDropAxis[R0]
  ): D2Case[A, Sem] { type F = F0; type S = S0; type R = R0 } =
    new PackedD2(sampled, dropAxis)

  def d3[
      A,
      Sem,
      F0 <: Frame[D3],
      S0 <: SampleSpace[F0, D3],
      R0 <: AnyRank
  ](
      sampled: Sampled[S0, A, Sem, R0]
  )(using
      dropAxis: CanDropAxis[R0]
  ): D3Case[A, Sem] { type F = F0; type S = S0; type R = R0 } =
    new PackedD3(sampled, dropAxis)

  private final class PackedD2[
      A,
      Sem,
      F0 <: Frame[D2],
      S0 <: SampleSpace[F0, D2],
      R0 <: AnyRank
  ](
      val value: Sampled[S0, A, Sem, R0],
      val dropAxisEvidence: CanDropAxis[R0]
  ) extends D2Case[A, Sem]:
    type F = F0
    type S = S0
    type R = R0

    val dimension: Dimension[D2] =
      Dimension[D2]

  private final class PackedD3[
      A,
      Sem,
      F0 <: Frame[D3],
      S0 <: SampleSpace[F0, D3],
      R0 <: AnyRank
  ](
      val value: Sampled[S0, A, Sem, R0],
      val dropAxisEvidence: CanDropAxis[R0]
  ) extends D3Case[A, Sem]:
    type F = F0
    type S = S0
    type R = R0

    val dimension: Dimension[D3] =
      Dimension[D3]
