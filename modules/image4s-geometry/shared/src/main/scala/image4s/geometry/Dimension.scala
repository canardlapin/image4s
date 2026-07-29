package image4s.geometry

sealed trait Dim
sealed trait D2 extends Dim
sealed trait D3 extends Dim

sealed abstract class Dimension[D <: Dim] private (
    val rank: Int
)

object Dimension:
  private object D2Dimension extends Dimension[D2](2)
  private object D3Dimension extends Dimension[D3](3)

  given d2: Dimension[D2] = D2Dimension
  given d3: Dimension[D3] = D3Dimension

  def apply[D <: Dim](using dimension: Dimension[D]): Dimension[D] =
    dimension
