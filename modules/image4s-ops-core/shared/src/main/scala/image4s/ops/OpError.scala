package image4s.ops

import image4s.ImageError
import image4s.geometry.GeometryError

/** Errors from image-aware operations over [[image4s.Sampled]].
  *
  * This module is temporarily hosted in the image4s repository. Extraction to
  * `canardlapin/image4s-ops` must remain mechanical: `image4s-core` never depends on `image4s.ops`.
  */
sealed trait OpError:
  def message: String

object OpError:
  final case class InvalidArgument(message: String) extends OpError

  final case class InvalidOffset(message: String) extends OpError

  final case class InvalidSupport(message: String) extends OpError

  final case class InvalidKernel(message: String) extends OpError

  final case class InvalidExtent(message: String) extends OpError

  final case class InvalidScale(message: String) extends OpError

  final case class Geometry(error: GeometryError) extends OpError:
    def message: String =
      error.message

  final case class Image(error: ImageError) extends OpError:
    def message: String =
      error.message
