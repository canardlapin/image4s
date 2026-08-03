package image4s

import ravel.AnyRank
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame

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

  final def spatialRank: Int =
    dimension.rank

  final def storageRank: Int =
    value.data.shape.rank

  def fold[B](
      onD2: SomeSampled.D2Case[A, Sem] => B,
      onD3: SomeSampled.D3Case[A, Sem] => B
  ): B

object SomeSampled:
  sealed trait D2Case[A, Sem] extends SomeSampled[A, Sem]:
    final type D = D2

    final def fold[B](
        onD2: D2Case[A, Sem] => B,
        onD3: D3Case[A, Sem] => B
    ): B =
      onD2(this)

  sealed trait D3Case[A, Sem] extends SomeSampled[A, Sem]:
    final type D = D3

    final def fold[B](
        onD2: D2Case[A, Sem] => B,
        onD3: D3Case[A, Sem] => B
    ): B =
      onD3(this)

  def d2[
      A,
      Sem,
      F0 <: Frame[D2],
      S0 <: SampleSpace[F0, D2],
      R0 <: AnyRank
  ](
      sampled: Sampled[S0, A, Sem, R0]
  ): D2Case[A, Sem] { type F = F0; type S = S0; type R = R0 } =
    new PackedD2(sampled)

  def d3[
      A,
      Sem,
      F0 <: Frame[D3],
      S0 <: SampleSpace[F0, D3],
      R0 <: AnyRank
  ](
      sampled: Sampled[S0, A, Sem, R0]
  ): D3Case[A, Sem] { type F = F0; type S = S0; type R = R0 } =
    new PackedD3(sampled)

  private final class PackedD2[
      A,
      Sem,
      F0 <: Frame[D2],
      S0 <: SampleSpace[F0, D2],
      R0 <: AnyRank
  ](
      val value: Sampled[S0, A, Sem, R0]
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
      val value: Sampled[S0, A, Sem, R0]
  ) extends D3Case[A, Sem]:
    type F = F0
    type S = S0
    type R = R0

    val dimension: Dimension[D3] =
      Dimension[D3]
