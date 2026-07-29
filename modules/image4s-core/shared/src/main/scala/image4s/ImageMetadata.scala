package image4s

/** Descriptive image metadata retained by the canonical sampled value.
  *
  * Metadata does not participate in grid identity or sampling compatibility.
  * The empty value is shared so unlabeled images do not require a distinct
  * metadata allocation.
  */
final case class ImageMetadata(label: String) derives CanEqual

object ImageMetadata:
  val empty: ImageMetadata =
    ImageMetadata("")

  def named(label: String): ImageMetadata =
    if label.isEmpty then empty else ImageMetadata(label)
