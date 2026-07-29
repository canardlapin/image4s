package image4s.laws

import image4s.FieldRole
import image4s.Sampled
import ravel.AnyRank
import image4s.geometry.Dim
import image4s.geometry.Frame

object ImageLaws:
  def shapeAgrees[
      F <: Frame[D],
      D <: Dim,
      A,
      Role <: FieldRole,
      R <: AnyRank
  ](
      sampled: Sampled[F, D, A, Role, R]
  ): Boolean =
    sampled.logicalShape ==
      Vector.tabulate(sampled.data.shape.rank)(sampled.data.shape.apply)

  def approximatelyEqual(
      left: Double,
      right: Double,
      tolerance: Double
  ): Boolean =
    math.abs(left - right) <= tolerance
