package image4s

import image4s.geometry.Dimension
import image4s.geometry.Grid
import ravel.AnyRank
import ravel.DType
import ravel.NDArray
import ravel.map

/** An immutable sampled image whose stored representation is decoded by a
  * structural [[ValueEncoding]].
  *
  * `Stored` describes Ravel storage; `Domain` is what callers observe through
  * [[valueAt]] and [[materializeTo]]. The wrapped raw `Sampled` owner keeps
  * image geometry, metadata, and zero-copy view behavior centralized.
  */
final class EncodedSampled[
    S <: SampleSpace[?, ?],
    StoredValue,
    Domain,
    Sem,
    R <: AnyRank
] private (
    val stored: Sampled[
      S,
      StoredValue,
      EncodedSampled.Stored,
      R
    ],
    val encoding: ValueEncoding[StoredValue, Domain],
    private val domainSemantics: ValueSemantics[Domain, Sem]
):
  inline def data: NDArray[StoredValue, R] =
    stored.data

  val sampleSpace: S =
    stored.sampleSpace

  inline def grid: Grid[sampleSpace.F, sampleSpace.D] =
    sampleSpace.grid

  inline def nonSpatialAxes: NonSpatialAxes =
    stored.nonSpatialAxes

  inline def metadata: ImageMetadata =
    stored.metadata

  inline def logicalShape: Vector[Int] =
    stored.logicalShape

  inline def fingerprint: String =
    encoding.fingerprint

  /** Read one decoded domain value without materializing the full image. */
  def valueAt(
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int] = Vector.empty
  ): Either[ImageError, Domain] =
    stored
      .valueAt(spatialIndex, nonSpatialIndex)
      .flatMap(
        encoding
          .decode(_, nonSpatialIndex)
          .left
          .map(ImageError.ValueEncoding.apply)
      )

  /** Decode into the domain dtype while retaining the complete sample space,
    * metadata, and semantic role.
    *
    * Identity and uniform-affine encodings use Ravel's shape-preserving
    * elementwise path. Coordinate-dependent and codebook encodings use the
    * same one-output-buffer builder while preserving C-order logical values.
    */
  def materializeTo(using
      domainDType: DType[Domain]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[sampleSpace.F, sampleSpace.D],
      Domain,
      Sem,
      R
    ]
  ] =
    decodeData.flatMap(decoded =>
      Sampled
        .create[Domain, Sem, R](sampleSpace, decoded, metadata)(
          using domainSemantics
        )
    )

  /** Apply an affine-correct zero-copy spatial crop to stored data. Encoding
    * coefficients retain their non-spatial alignment unchanged.
    */
  def spatialView(
      origin: Vector[Int],
      shape: Vector[Int]
  )(using dimension: Dimension[sampleSpace.D]): Either[
    ImageError,
    EncodedSampled[
      ? <: SampleSpace[?, ?],
      StoredValue,
      Domain,
      Sem,
      R
    ]
  ] =
    stored
      .spatialView(origin, shape)(
        using dimension.asInstanceOf[Dimension[stored.sampleSpace.D]]
      )
      .map(viewed =>
        new EncodedSampled(
          viewed,
          encoding,
          domainSemantics
        )
      )

  def crop(
      origin: Vector[Int],
      shape: Vector[Int]
  )(using dimension: Dimension[sampleSpace.D]): Either[
    ImageError,
    EncodedSampled[
      ? <: SampleSpace[?, ?],
      StoredValue,
      Domain,
      Sem,
      R
    ]
  ] =
    spatialView(origin, shape)

  private def decodeData(using
      domainDType: DType[Domain]
  ): Either[ImageError, NDArray[Domain, R]] =
    encoding match
      case _: ValueEncoding.Identity[?] =>
        Right(data.asInstanceOf[NDArray[Domain, R]])
      case affine: ValueEncoding.UniformAffine =>
        val source = data.asInstanceOf[NDArray[Double, R]]
        Right(
          source
            .map(value => value * affine.slope + affine.intercept)
            .asInstanceOf[NDArray[Domain, R]]
        )
      case _ =>
        decodeCoordinateAware

  private def decodeCoordinateAware(using
      domainDType: DType[Domain]
  ): Either[ImageError, NDArray[Domain, R]] =
    var failure: Option[EncodingError] = None
    var linear = 0
    val decoded = NDArray.build[Domain, R](data.shape) { builder =>
      data.foreachIndex { index =>
        if failure.isEmpty then
          val nonSpatialIndex =
            Vector.tabulate(nonSpatialAxes.size)(axis =>
              index(grid.spatialRank + axis)
            )
          encoding.decode(data.at(index), nonSpatialIndex) match
            case Right(value) =>
              builder.writeLinear(linear, value)
            case Left(error) =>
              failure = Some(error)
        linear += 1
      }
    }
    failure match
      case Some(error) => Left(ImageError.ValueEncoding(error))
      case None        => Right(decoded)

object EncodedSampled:
  /** Internal semantic tag for stored values. It never exposes a second public
    * image meaning: callers interact with `Domain` and `Sem`.
    */
  sealed trait Stored

  private given storedSemantics[A]: ValueSemantics[A, Stored] with {}

  def create[
      StoredValue,
      Domain,
      Sem,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[StoredValue, R],
      encoding: ValueEncoding[StoredValue, Domain],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      domainSemantics: ValueSemantics[Domain, Sem]
  ): Either[
    ImageError,
    EncodedSampled[
      sampleSpace.type,
      StoredValue,
      Domain,
      Sem,
      R
    ]
  ] =
    encoding
      .validateFor(sampleSpace.nonSpatialAxes.shape)
      .left
      .map(ImageError.ValueEncoding.apply)
      .flatMap(_ =>
        Sampled
          .create[StoredValue, Stored, R](sampleSpace, data, metadata)
          .map(raw =>
            new EncodedSampled(
              raw,
              encoding,
              domainSemantics
            )
          )
      )

  def identity[
      A,
      Sem,
      R <: AnyRank
  ](
      sampleSpace: SampleSpace[?, ?],
      data: NDArray[A, R],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      semantics: ValueSemantics[A, Sem]
  ): Either[
    ImageError,
    EncodedSampled[sampleSpace.type, A, A, Sem, R]
  ] =
    create(sampleSpace, data, ValueEncoding.Identity[A](), metadata)
