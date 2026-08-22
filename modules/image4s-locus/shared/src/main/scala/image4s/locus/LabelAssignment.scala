package image4s.locus

import scala.annotation.implicitNotFound

import image4s.Categorical
import image4s.ImageMetadata
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Dim
import image4s.geometry.Frame
import locus4s.CertifiedMapError
import locus4s.DomainDescriptor
import locus4s.DomainError
import locus4s.DomainFingerprint
import locus4s.DomainId
import locus4s.DomainKey
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainRestoreError
import locus4s.FiniteDomain
import locus4s.FiniteSpace
import locus4s.PartialMapError
import locus4s.PartialSurjection
import locus4s.data.Field
import locus4s.data.FieldConstructionError
import locus4s.data.VectorField
import ravel.AnyRank
import ravel.UInt16
import ravel.UInt8

/** Exact widening of one supported integral image label to its semantic integer code.
  *
  * The original value remains in [[LabelDefinition]]. Widening is used only for deterministic
  * lookup, diagnostics, and persistent target fingerprints. Equal numeric codes therefore keep the
  * same target identity when an image is losslessly stored with a different integral dtype.
  */
@implicitNotFound(
  "${A} is not a supported integral image label. Use Byte, UInt8, Short, UInt16, Int, or Long."
)
sealed trait LabelCode[A]:
  def toLong(value: A): Long

object LabelCode:
  given LabelCode[Byte] with
    def toLong(value: Byte): Long = value.toLong

  given LabelCode[UInt8] with
    def toLong(value: UInt8): Long = value.toLong

  given LabelCode[Short] with
    def toLong(value: Short): Long = value.toLong

  given LabelCode[UInt16] with
    def toLong(value: UInt16): Long = value.toLong

  given LabelCode[Int] with
    def toLong(value: Int): Long = value.toLong

  given LabelCode[Long] with
    def toLong(value: Long): Long = value

/** One target label in target-domain order.
  *
  * `value` is the exact integer value stored in the categorical image. `name` and `metadata` are
  * presentation/provenance data: they are retained by assignment discovery but do not become a
  * second voxel-to-target representation.
  */
final case class LabelDefinition[A, M](
    value: A,
    name: String,
    metadata: M
)

/** Policy for an image value that is neither an explicit background value nor a target label. */
enum UnknownLabelPolicy derives CanEqual:
  case Reject
  case TreatAsBackground

/** Explicit categorical-image decoding policy.
  *
  * An empty `backgroundValues` vector explicitly declares that no integer value is background. No
  * implicit zero/background convention is applied.
  */
final case class LabelAssignmentPolicy[A](
    backgroundValues: Vector[A],
    unknownLabels: UnknownLabelPolicy
)

/** Version tag for persisted label-assignment construction evidence. */
enum LabelAssignmentSchema(val id: String) derives CanEqual:
  case V1 extends LabelAssignmentSchema("image4s-label-assignment/v1")
  case Unrecognized(value: String) extends LabelAssignmentSchema(value)

/** Versioned evidence sufficient to reconstruct one categorical label assignment.
  *
  * The locus4s [[PartialSurjection]] remains the authoritative assignment. This record retains the
  * source grid-domain identity, target identity, ordered integer keys, names, metadata, image
  * metadata, and decoding policy without duplicating the per-voxel assignment.
  */
final case class LabelAssignmentRecord[A, M](
    source: GridDomainRecord,
    target: DomainRecord,
    orderedLabels: Vector[LabelDefinition[A, M]],
    policy: LabelAssignmentPolicy[A],
    imageMetadata: ImageMetadata,
    schema: LabelAssignmentSchema
)

sealed trait LabelAssignmentError:
  def message: String

object LabelAssignmentError:
  final case class InvalidLabelName(
      targetOrdinal: Int,
      name: String
  ) extends LabelAssignmentError:
    val message: String =
      s"target label $targetOrdinal has a blank or padded name: '$name'"

  final case class DuplicateLabelValue(
      value: Long,
      firstTargetOrdinal: Int,
      secondTargetOrdinal: Int
  ) extends LabelAssignmentError:
    val message: String =
      s"integer label $value names both target $firstTargetOrdinal and " +
        s"target $secondTargetOrdinal"

  final case class DuplicateBackgroundValue(value: Long) extends LabelAssignmentError:
    val message: String =
      s"background integer label $value is repeated"

  final case class BackgroundLabelConflict(
      value: Long,
      targetOrdinal: Int
  ) extends LabelAssignmentError:
    val message: String =
      s"integer label $value is both background and target $targetOrdinal"

  final case class ForeignDecodedLabelOwner(
      expected: DomainDescriptor,
      actual: DomainDescriptor
  ) extends LabelAssignmentError:
    val message: String =
      s"decoded label indices belong to a foreign target owner: expected $expected, found $actual"

  final case class TargetLabelCountMismatch(
      targetSize: Int,
      labelCount: Int
  ) extends LabelAssignmentError:
    val message: String =
      s"target domain has size $targetSize, but the ordered label field has $labelCount values"

  final case class PersistentTargetFingerprintRequired(
      target: DomainKey
  ) extends LabelAssignmentError:
    val message: String =
      s"persistent label target ${target.id.value} requires an ordered-label fingerprint"

  final case class TargetFingerprintMismatch(
      target: DomainKey,
      expected: DomainFingerprint,
      actual: DomainFingerprint
  ) extends LabelAssignmentError:
    val message: String =
      s"persistent label target ${target.id.value} has fingerprint '${actual.value}', " +
        s"expected '${expected.value}'"

  final case class UnknownImageLabel(
      sourceOrdinal: Int,
      value: Long
  ) extends LabelAssignmentError:
    val message: String =
      s"source ordinal $sourceOrdinal has unknown integer label $value"

  final case class MissingTargetLabel(
      targetOrdinal: Int,
      value: Long,
      name: String
  ) extends LabelAssignmentError:
    val message: String =
      s"target $targetOrdinal ('$name', integer label $value) has no assigned source"

  final case class WrongDecodedValueCount(
      expected: Int,
      actual: Int
  ) extends LabelAssignmentError:
    val message: String =
      s"decoded assignment requires $expected source values, found $actual"

  final case class InvalidDecodedTargetOrdinal(
      sourceOrdinal: Int,
      targetOrdinal: Int,
      targetSize: Int
  ) extends LabelAssignmentError:
    val message: String =
      s"source ordinal $sourceOrdinal decoded to target $targetOrdinal outside [0, $targetSize)"

  final case class InsufficientSourceCoverage(
      sourceSize: Int,
      targetSize: Int
  ) extends LabelAssignmentError:
    val message: String =
      s"$sourceSize source voxels cannot cover $targetSize target labels"

  final case class InvalidCertifiedAssignment(
      error: CertifiedMapError
  ) extends LabelAssignmentError:
    val message: String = error.message

  final case class GridFailure(error: GridDomainError) extends LabelAssignmentError:
    val message: String = error.message

  final case class DomainFailure(error: DomainError) extends LabelAssignmentError:
    val message: String = error.message

  final case class RegistryFailure(error: DomainRestoreError) extends LabelAssignmentError:
    val message: String = error.message

  final case class LabelFieldFailure(error: FieldConstructionError) extends LabelAssignmentError:
    val message: String = error.message

  final case class UnsupportedRecordSchema(
      actual: LabelAssignmentSchema
  ) extends LabelAssignmentError:
    val message: String =
      s"unsupported label-assignment record schema '${actual.id}'"

  final case class SourceRecordMismatch(
      expected: GridDomainRecord,
      actual: GridDomainRecord
  ) extends LabelAssignmentError:
    val message: String =
      s"label-assignment evidence names a different source grid domain: expected $expected, found $actual"

  final case class ImageMetadataMismatch(
      expected: ImageMetadata,
      actual: ImageMetadata
  ) extends LabelAssignmentError:
    val message: String =
      s"label-assignment evidence names different image metadata: expected $expected, found $actual"

/** A discovered persistent target domain and its authoritative assignment.
  *
  * The abstract `P` keeps the generated target owner existential. `target`, `labels`, and
  * `assignment` share that exact owner, while `registry` is the immutable registry containing it.
  */
sealed trait DiscoveredLabelAssignment[V, A, M]:
  type P
  val registry: DomainRegistry
  val target: FiniteSpace[P]
  val labels: Field[P, LabelDefinition[A, M]]
  val assignment: PartialSurjection[V, P]
  val record: LabelAssignmentRecord[A, M]

object LabelAssignment:
  val RecordSchema: LabelAssignmentSchema =
    LabelAssignmentSchema.V1

  val TargetFingerprintSchema: String =
    "image4s-label-domain-key/v1"

  /** Construct the canonical persistent target record for an explicit ordered label table.
    *
    * Target identity depends on the ordered semantic integer codes, not image storage dtype, label
    * names, or label metadata.
    */
  def canonicalTargetRecord[A, M](
      id: DomainId,
      name: String,
      orderedLabels: IterableOnce[LabelDefinition[A, M]]
  )(using code: LabelCode[A]): Either[LabelAssignmentError, DomainRecord] =
    validateLabels(orderedLabels.iterator.toVector).flatMap: labels =>
      makeTargetRecord(id, name, labels)

  /** Convert one spatial-only categorical image using a caller-supplied target domain.
    *
    * `labels` is indexed in target-domain order. Its live owner must be exactly `target`; an
    * independently restored same-key owner is deliberately rejected instead of being aligned
    * implicitly. Persistent targets must carry this bridge's ordered-code fingerprint.
    */
  def toPartialSurjection[
      F <: Frame[D],
      D <: Dim,
      V,
      I <: SampleSpace[?, ?],
      A,
      M,
      R <: AnyRank,
      P,
      Q
  ](
      bridge: GridDomain[F, D, V],
      image: Sampled[I, A, Categorical, R],
      target: FiniteDomain[P],
      labels: Field[Q, LabelDefinition[A, M]],
      policy: LabelAssignmentPolicy[A]
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    PartialSurjection[V, P]
  ] =
    if !target.sameRuntimeOwnerAs(labels.space) then
      Left(
        LabelAssignmentError.ForeignDecodedLabelOwner(
          target.descriptor,
          labels.space.descriptor
        )
      )
    else
      for
        catalog <- validateCatalog(labels.toVector, policy)
        _ <- validateTarget(target, catalog)
        field <- bridge
          .spatialField(image)
          .left
          .map(LabelAssignmentError.GridFailure.apply)
        assignment <- decode(field, target, catalog)
      yield assignment

  /** Create or converge on a persistent target domain derived from explicit ordered labels.
    *
    * The caller supplies the persistent `targetId`; image4s derives the structural fingerprint and
    * never mints an identifier. The result returns the existential target, its ordered label field,
    * the registry that owns it, the authoritative partial surjection, and reconstruction evidence.
    */
  def discover[
      F <: Frame[D],
      D <: Dim,
      V,
      I <: SampleSpace[?, ?],
      A,
      M,
      R <: AnyRank
  ](
      bridge: GridDomain[F, D, V],
      image: Sampled[I, A, Categorical, R],
      targetId: DomainId,
      targetName: String,
      orderedLabels: IterableOnce[LabelDefinition[A, M]],
      policy: LabelAssignmentPolicy[A],
      registry: DomainRegistry
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    DiscoveredLabelAssignment[V, A, M]
  ] =
    for
      catalog <- validateCatalog(orderedLabels.iterator.toVector, policy)
      targetRecord <- makeTargetRecord(targetId, targetName, catalog.labels)
      record = LabelAssignmentRecord(
        bridge.record,
        targetRecord,
        catalog.labels.values,
        policy,
        image.metadata,
        RecordSchema
      )
      result <- resolve(record, bridge, image, registry, catalog)
    yield result

  /** Restore a discovered assignment through a supplied immutable registry.
    *
    * Source grid identity, image metadata, ordered target keys, and schema are checked before the
    * target owner is restored. A fresh registry yields a distinct live owner with the same
    * persistent key; the returned field and assignment use that new owner consistently.
    */
  def restore[
      F <: Frame[D],
      D <: Dim,
      V,
      I <: SampleSpace[?, ?],
      A,
      M,
      R <: AnyRank
  ](
      record: LabelAssignmentRecord[A, M],
      bridge: GridDomain[F, D, V],
      image: Sampled[I, A, Categorical, R],
      registry: DomainRegistry
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    DiscoveredLabelAssignment[V, A, M]
  ] =
    for
      _ <- Either.cond(
        record.schema == RecordSchema,
        (),
        LabelAssignmentError.UnsupportedRecordSchema(record.schema)
      )
      _ <- Either.cond(
        record.source == bridge.record,
        (),
        LabelAssignmentError.SourceRecordMismatch(
          record.source,
          bridge.record
        )
      )
      _ <- Either.cond(
        record.imageMetadata == image.metadata,
        (),
        LabelAssignmentError.ImageMetadataMismatch(
          record.imageMetadata,
          image.metadata
        )
      )
      catalog <- validateCatalog(record.orderedLabels, record.policy)
      _ <- validateTargetKey(record.target.key, catalog)
      result <- resolve(record, bridge, image, registry, catalog)
    yield result

  private final class Resolved[V, A, M, T](
      val registry: DomainRegistry,
      val target: FiniteSpace[T],
      val labels: Field[T, LabelDefinition[A, M]],
      val assignment: PartialSurjection[V, T],
      val record: LabelAssignmentRecord[A, M]
  ) extends DiscoveredLabelAssignment[V, A, M]:
    type P = T

  private final case class ValidatedLabels[A, M](
      values: Vector[LabelDefinition[A, M]],
      targetOrdinalByValue: Map[Long, Int],
      fingerprint: DomainFingerprint
  )

  private final case class ValidatedCatalog[A, M](
      labels: ValidatedLabels[A, M],
      backgroundValues: Set[Long],
      unknownLabels: UnknownLabelPolicy
  )

  private def validateLabels[A, M](
      labels: Vector[LabelDefinition[A, M]]
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    ValidatedLabels[A, M]
  ] =
    var targetOrdinalByValue = Map.empty[Long, Int]
    var targetOrdinal = 0
    var failure = Option.empty[LabelAssignmentError]
    while targetOrdinal < labels.size && failure.isEmpty do
      val label = labels(targetOrdinal)
      val numericValue = code.toLong(label.value)
      if label.name.isEmpty || label.name != label.name.trim then
        failure = Some(
          LabelAssignmentError.InvalidLabelName(
            targetOrdinal,
            label.name
          )
        )
      else
        targetOrdinalByValue.get(numericValue) match
          case Some(firstTargetOrdinal) =>
            failure = Some(
              LabelAssignmentError.DuplicateLabelValue(
                numericValue,
                firstTargetOrdinal,
                targetOrdinal
              )
            )
          case None =>
            targetOrdinalByValue = targetOrdinalByValue.updated(numericValue, targetOrdinal)
      targetOrdinal += 1

    failure match
      case Some(error) => Left(error)
      case None =>
        targetFingerprint(
          labels.map(label => code.toLong(label.value))
        ).map: fingerprint =>
          ValidatedLabels(
            labels,
            targetOrdinalByValue,
            fingerprint
          )

  private def validateCatalog[A, M](
      labels: Vector[LabelDefinition[A, M]],
      policy: LabelAssignmentPolicy[A]
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    ValidatedCatalog[A, M]
  ] =
    validateLabels(labels).flatMap: validatedLabels =>
      var backgroundValues = Set.empty[Long]
      val values = policy.backgroundValues.iterator
      var failure = Option.empty[LabelAssignmentError]
      while values.hasNext && failure.isEmpty do
        val value = code.toLong(values.next())
        if backgroundValues.contains(value) then
          failure = Some(
            LabelAssignmentError.DuplicateBackgroundValue(value)
          )
        else
          validatedLabels.targetOrdinalByValue.get(value) match
            case Some(targetOrdinal) =>
              failure = Some(
                LabelAssignmentError.BackgroundLabelConflict(
                  value,
                  targetOrdinal
                )
              )
            case None =>
              backgroundValues = backgroundValues + value

      failure match
        case Some(error) => Left(error)
        case None =>
          Right(
            ValidatedCatalog(
              validatedLabels,
              backgroundValues,
              policy.unknownLabels
            )
          )

  private def validateTarget[P, A, M](
      target: FiniteDomain[P],
      catalog: ValidatedCatalog[A, M]
  ): Either[LabelAssignmentError, Unit] =
    if target.size != catalog.labels.values.size then
      Left(
        LabelAssignmentError.TargetLabelCountMismatch(
          target.size,
          catalog.labels.values.size
        )
      )
    else
      target.persistentKey match
        case Some(key) => validateTargetKey(key, catalog)
        case None => Right(())

  private def validateTargetKey[A, M](
      key: DomainKey,
      catalog: ValidatedCatalog[A, M]
  ): Either[LabelAssignmentError, Unit] =
    if key.size != catalog.labels.values.size then
      Left(
        LabelAssignmentError.TargetLabelCountMismatch(
          key.size,
          catalog.labels.values.size
        )
      )
    else
      key.fingerprint match
        case None =>
          Left(
            LabelAssignmentError.PersistentTargetFingerprintRequired(key)
          )
        case Some(actual) if actual == catalog.labels.fingerprint =>
          Right(())
        case Some(actual) =>
          Left(
            LabelAssignmentError.TargetFingerprintMismatch(
              key,
              catalog.labels.fingerprint,
              actual
            )
          )

  private def makeTargetRecord[A, M](
      id: DomainId,
      name: String,
      labels: ValidatedLabels[A, M]
  ): Either[LabelAssignmentError, DomainRecord] =
    DomainRecord
      .make(
        id,
        name,
        labels.values.size,
        Some(labels.fingerprint)
      )
      .left
      .map(LabelAssignmentError.DomainFailure.apply)

  private def decode[
      V,
      I <: SampleSpace[?, ?],
      A,
      M,
      R <: AnyRank,
      P
  ](
      field: SpatialFieldView[V, I, A, Categorical, R],
      target: FiniteDomain[P],
      catalog: ValidatedCatalog[A, M]
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    PartialSurjection[V, P]
  ] =
    val targetOrdinals = Array.fill[Option[Int]](field.space.size)(None)
    var failure = Option.empty[LabelAssignmentError]
    field.space.foreachIndex: source =>
      if failure.isEmpty then
        val numericValue = code.toLong(field(source))
        if catalog.backgroundValues.contains(numericValue) then
          targetOrdinals(source.ordinal) = None
        else
          catalog.labels.targetOrdinalByValue.get(numericValue) match
            case Some(targetOrdinal) =>
              if targetOrdinal >= 0 && targetOrdinal < target.size then
                targetOrdinals(source.ordinal) = Some(targetOrdinal)
              else
                failure = Some(
                  LabelAssignmentError.InvalidDecodedTargetOrdinal(
                    source.ordinal,
                    targetOrdinal,
                    target.size
                  )
                )
            case None =>
              catalog.unknownLabels match
                case UnknownLabelPolicy.Reject =>
                  failure = Some(
                    LabelAssignmentError.UnknownImageLabel(
                      source.ordinal,
                      numericValue
                    )
                  )
                case UnknownLabelPolicy.TreatAsBackground =>
                  targetOrdinals(source.ordinal) = None

    failure match
      case Some(error) => Left(error)
      case None =>
        PartialSurjection
          .fromOptionalTargetOrdinals(
            field.space,
            target,
            targetOrdinals
          )
          .left
          .map:
            case PartialMapError.WrongTargetCount(expected, actual) =>
              LabelAssignmentError.WrongDecodedValueCount(
                expected,
                actual
              )
            case PartialMapError.TargetOutOfBounds(source, target, size) =>
              LabelAssignmentError.InvalidDecodedTargetOrdinal(
                source,
                target,
                size
              )
            case error @ CertifiedMapError.NotSurjective(missing) =>
              catalog.labels.values.lift(missing) match
                case Some(label) =>
                  LabelAssignmentError.MissingTargetLabel(
                    missing,
                    code.toLong(label.value),
                    label.name
                  )
                case None =>
                  LabelAssignmentError.InvalidCertifiedAssignment(error)
            case CertifiedMapError.InsufficientSources(fromSize, toSize) =>
              LabelAssignmentError.InsufficientSourceCoverage(
                fromSize,
                toSize
              )
            case error @ CertifiedMapError.NotInjective(_, _, _) =>
              LabelAssignmentError.InvalidCertifiedAssignment(error)
            case error @ CertifiedMapError.DifferentCardinality(_, _) =>
              LabelAssignmentError.InvalidCertifiedAssignment(error)

  private def resolve[
      F <: Frame[D],
      D <: Dim,
      V,
      I <: SampleSpace[?, ?],
      A,
      M,
      R <: AnyRank
  ](
      record: LabelAssignmentRecord[A, M],
      bridge: GridDomain[F, D, V],
      image: Sampled[I, A, Categorical, R],
      registry: DomainRegistry,
      catalog: ValidatedCatalog[A, M]
  )(using
      code: LabelCode[A]
  ): Either[
    LabelAssignmentError,
    DiscoveredLabelAssignment[V, A, M]
  ] =
    registry
      .restore(record.target)
      .left
      .map(LabelAssignmentError.RegistryFailure.apply)
      .flatMap: targetResolution =>
        VectorField
          .fromValues(
            targetResolution.space,
            catalog.labels.values
          )
          .left
          .map(LabelAssignmentError.LabelFieldFailure.apply)
          .flatMap: labelField =>
            toPartialSurjection(
              bridge,
              image,
              targetResolution.space,
              labelField,
              record.policy
            ).map: assignment =>
              new Resolved(
                targetResolution.registry,
                targetResolution.space,
                labelField,
                assignment,
                record.copy(target = targetResolution.space.record)
              )

  private def targetFingerprint(
      values: Vector[Long]
  ): Either[LabelAssignmentError, DomainFingerprint] =
    val encoded =
      Vector(
        component("schema", TargetFingerprintSchema),
        component(
          "ordered-codes",
          values.map(value => component("code", value.toString)).mkString("|")
        )
      ).mkString("|")
    DomainFingerprint
      .parse(encoded)
      .left
      .map(LabelAssignmentError.DomainFailure.apply)

  private def component(name: String, value: String): String =
    s"$name:${value.length}:$value"
