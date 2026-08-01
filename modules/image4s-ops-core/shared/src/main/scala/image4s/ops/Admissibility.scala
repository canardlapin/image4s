package image4s.ops

import scala.annotation.implicitNotFound
import image4s.Continuous

/** Evidence that values with semantic tag `Sem` may be linearly filtered.
  *
  * There is deliberately no instance for [[image4s.Categorical]] or
  * [[image4s.Mask]].
  */
@implicitNotFound(
  "Semantic tag ${Sem} does not support linear filtering. Continuous fields do; Label/Mask/Categorical do not."
)
trait SupportsLinearFiltering[Sem]

object SupportsLinearFiltering:
  given SupportsLinearFiltering[Continuous] with {}
