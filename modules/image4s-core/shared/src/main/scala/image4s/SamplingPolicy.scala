package image4s

enum BoundaryPolicy[+A] derives CanEqual:
  case Reject
  case Constant(value: A)

opaque type PartialWeight = Double

object PartialWeight:
  def from(value: Double): Either[ImageError, PartialWeight] =
    if value.isFinite && value > 0.0 && value < 1.0 then Right(value)
    else Left(ImageError.InvalidPartialWeight(value))

  extension (weight: PartialWeight)
    def value: Double = weight

sealed trait Validity derives CanEqual

object Validity:
  case object Full extends Validity
  final case class Partial(insideWeight: PartialWeight) extends Validity
  case object Outside extends Validity

final case class Sample[+A](
    value: A,
    validity: Validity
) derives CanEqual
