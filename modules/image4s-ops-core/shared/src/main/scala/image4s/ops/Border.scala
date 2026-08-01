package image4s.ops

/** Border handling for whole-image neighborhood operations.
  *
  * Distinct from point-sampling [[image4s.BoundaryPolicy]]. Avoid the bare
  * word "Mirror": libraries disagree on whether the edge sample is repeated.
  */
enum Border[+A] derives CanEqual:
  case Constant(value: A)
  case Replicate
  case ReflectWithoutEdge
  case ReflectWithEdge
  case Wrap

object Border:
  def reflect: Border[Nothing] =
    ReflectWithoutEdge
