package image4s.ops

/** Declares whether a differential / geometric result is expressed in index
  * coordinates or physical frame coordinates.
  */
sealed trait CoordinateDomain derives CanEqual

case object IndexCoordinates extends CoordinateDomain
case object FrameCoordinates extends CoordinateDomain
