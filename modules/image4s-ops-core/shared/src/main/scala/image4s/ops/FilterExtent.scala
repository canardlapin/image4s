package image4s.ops

/** Output spatial extent for a neighborhood filter.
  *
  * `Valid` cannot carry a border mode: there is no exterior to fill.
  */
enum FilterExtent[+A] derives CanEqual:
  case Same(border: Border[A])
  case Valid
  case Full(border: Border[A])

object FilterExtent:
  def same[A](border: Border[A]): FilterExtent[A] =
    Same(border)

  def valid: FilterExtent[Nothing] =
    Valid

  def full[A](border: Border[A]): FilterExtent[A] =
    Full(border)
