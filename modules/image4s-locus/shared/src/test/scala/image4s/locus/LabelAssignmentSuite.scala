package image4s.locus

import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D2
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import locus4s.DomainId
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.data.VectorField
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class LabelAssignmentSuite extends FunSuite:
  private final case class ParcelMetadata(
      color: String,
      network: Option[String]
  )

  private val labels =
    Vector(
      LabelDefinition(
        20,
        "twenty",
        ParcelMetadata("#cc0000", Some("control"))
      ),
      LabelDefinition(
        10,
        "ten",
        ParcelMetadata("#0000cc", Some("visual"))
      )
    )

  private val permissivePolicy =
    LabelAssignmentPolicy(
      backgroundValues = Vector(0),
      unknownLabels = UnknownLabelPolicy.TreatAsBackground
    )

  test("caller-supplied conversion follows explicit target and asymmetric grid order"):
    val grid = persistentGrid2("supplied", Vector(2, 3))
    val bridge = register(grid, "source voxels").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 99, 10)
      ),
      "atlas source"
    )
    val targetRecord = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-supplied"),
        "supplied parcels",
        labels
      )
    )
    val target = right(DomainRegistry.empty.restore(targetRecord))
    val labelField = right(VectorField.fromValues(target.space, labels))
    val assignment = right(
      LabelAssignment.toPartialSurjection(
        bridge,
        image,
        target.space,
        labelField,
        permissivePolicy
      )
    )

    assertEquals(
      assignment.toPartialMap.optionalTargetOrdinals,
      Vector(None, Some(0), Some(1), Some(0), None, Some(1))
    )
    assertEquals(
      assignment.support.ordinalsInDomainOrder.toVector,
      Vector(1, 2, 3, 5)
    )
    assertEquals(
      right(assignment.fibers).ordinalRows.map(_.toVector).toVector,
      Vector(Vector(1, 3), Vector(2, 5))
    )
    assertEquals(labelField.toVector, labels)

  test("unknown labels reject with their row-major source ordinal"):
    val grid = persistentGrid2("unknown", Vector(2, 3))
    val bridge = register(grid, "unknown source").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 99, 10)
      ),
      "unknown source"
    )
    val targetRecord = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-unknown"),
        "unknown parcels",
        labels
      )
    )
    val target = right(DomainRegistry.empty.restore(targetRecord))
    val labelField = right(VectorField.fromValues(target.space, labels))
    val policy =
      LabelAssignmentPolicy(
        backgroundValues = Vector(0),
        unknownLabels = UnknownLabelPolicy.Reject
      )

    assertEquals(
      LabelAssignment.toPartialSurjection(
        bridge,
        image,
        target.space,
        labelField,
        policy
      ),
      Left(LabelAssignmentError.UnknownImageLabel(4, 99L))
    )

  test("every ordered target label must have an image preimage"):
    val grid = persistentGrid2("coverage", Vector(2, 3))
    val bridge = register(grid, "coverage source").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 0, 10)
      ),
      "coverage source"
    )
    val labelsWithMissing =
      labels :+ LabelDefinition(
        30,
        "thirty",
        ParcelMetadata("#00cc00", None)
      )
    val targetRecord = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-coverage"),
        "coverage parcels",
        labelsWithMissing
      )
    )
    val target = right(DomainRegistry.empty.restore(targetRecord))
    val labelField =
      right(VectorField.fromValues(target.space, labelsWithMissing))

    assertEquals(
      LabelAssignment.toPartialSurjection(
        bridge,
        image,
        target.space,
        labelField,
        permissivePolicy
      ),
      Left(
        LabelAssignmentError.MissingTargetLabel(
          2,
          30L,
          "thirty"
        )
      )
    )

  test("same-key labels restored by a foreign registry owner are rejected"):
    val grid = persistentGrid2("foreign", Vector(2, 3))
    val bridge = register(grid, "foreign source").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 0, 10)
      ),
      "foreign source"
    )
    val targetRecord = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-foreign"),
        "foreign parcels",
        labels
      )
    )
    val expected = right(DomainRegistry.empty.restore(targetRecord))
    val foreign = right(DomainRegistry.empty.restore(targetRecord))
    val foreignLabels =
      right(VectorField.fromValues(foreign.space, labels))

    assert(!expected.space.sameRuntimeOwnerAs(foreign.space))
    LabelAssignment.toPartialSurjection(
      bridge,
      image,
      expected.space,
      foreignLabels,
      permissivePolicy
    ) match
      case Left(LabelAssignmentError.ForeignDecodedLabelOwner(wanted, actual)) =>
        assertEquals(wanted, expected.space.descriptor)
        assertEquals(actual, foreign.space.descriptor)
      case result =>
        fail(s"expected foreign decoded-label owner, found $result")

  test("persistent targets require the canonical ordered-code fingerprint"):
    val grid = persistentGrid2("fingerprint", Vector(2, 3))
    val bridge = register(grid, "fingerprint source").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 0, 10)
      ),
      "fingerprint source"
    )
    val wrongRecord = right(
      DomainRecord.parse(
        "parcels-fingerprint",
        "fingerprint parcels",
        labels.size,
        Some("wrong-ordered-label-fingerprint")
      )
    )
    val target = right(DomainRegistry.empty.restore(wrongRecord))
    val labelField = right(VectorField.fromValues(target.space, labels))

    LabelAssignment.toPartialSurjection(
      bridge,
      image,
      target.space,
      labelField,
      permissivePolicy
    ) match
      case Left(_: LabelAssignmentError.TargetFingerprintMismatch) => ()
      case result => fail(s"expected target fingerprint mismatch, found $result")

  test("discovery retains label evidence and restores registry-scoped identity"):
    val grid = persistentGrid2("discovery", Vector(2, 3))
    val bridge = register(grid, "discovery source").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 0, 10)
      ),
      "discovery image"
    )
    val policy =
      LabelAssignmentPolicy(
        backgroundValues = Vector(0),
        unknownLabels = UnknownLabelPolicy.Reject
      )
    val id = domainId("parcels-discovery")
    val discovered = right(
      LabelAssignment.discover(
        bridge,
        image,
        id,
        "discovered parcels",
        labels,
        policy,
        DomainRegistry.empty
      )
    )

    assertEquals(discovered.labels.toVector, labels)
    assertEquals(discovered.record.source, bridge.record)
    assertEquals(discovered.record.target, discovered.target.record)
    assertEquals(discovered.record.orderedLabels, labels)
    assertEquals(discovered.record.policy, policy)
    assertEquals(discovered.record.imageMetadata, image.metadata)
    assertEquals(discovered.record.schema, LabelAssignment.RecordSchema)
    assertEquals(
      discovered.assignment.toPartialMap.optionalTargetOrdinals,
      Vector(None, Some(0), Some(1), Some(0), None, Some(1))
    )
    val registered =
      discovered.registry.find(id).getOrElse(fail("target was not registered"))
    assert(registered.value.sameRuntimeOwnerAs(discovered.target))

    val converged = right(
      LabelAssignment.discover(
        bridge,
        image,
        id,
        "discovered parcels",
        labels,
        policy,
        discovered.registry
      )
    )
    assert(discovered.target.sameRuntimeOwnerAs(converged.target))

    val restored = right(
      LabelAssignment.restore(
        discovered.record,
        bridge,
        image,
        DomainRegistry.empty
      )
    )
    assert(!discovered.target.sameRuntimeOwnerAs(restored.target))
    assertEquals(discovered.target.key, restored.target.key)
    assert(discovered.target.align(restored.target).isRight)
    assertEquals(restored.labels.toVector, labels)
    assertEquals(
      restored.assignment.toPartialMap.optionalTargetOrdinals,
      discovered.assignment.toPartialMap.optionalTargetOrdinals
    )

    val futureRecord =
      discovered.record.copy(
        schema = LabelAssignmentSchema.Unrecognized(
          "image4s-label-assignment/v9"
        )
      )
    LabelAssignment.restore(
      futureRecord,
      bridge,
      image,
      DomainRegistry.empty
    ) match
      case Left(LabelAssignmentError.UnsupportedRecordSchema(actual)) =>
        assertEquals(actual, futureRecord.schema)
      case result =>
        fail(s"expected unsupported record schema, found $result")

  test("reordering target labels changes ordinals but preserves the partition"):
    val grid = persistentGrid2("reorder", Vector(2, 3))
    val bridge = register(grid, "reorder source").value
    val image = categoricalImage(
      grid,
      Vector(
        Vector(0, 20, 10),
        Vector(20, 0, 10)
      ),
      "reorder source"
    )
    val reversedLabels = labels.reverse

    val firstRecord = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-reorder"),
        "ordered parcels",
        labels
      )
    )
    val secondRecord = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-reorder"),
        "reordered parcels",
        reversedLabels
      )
    )
    val firstTarget = right(DomainRegistry.empty.restore(firstRecord))
    val secondTarget = right(DomainRegistry.empty.restore(secondRecord))
    val firstLabels = right(VectorField.fromValues(firstTarget.space, labels))
    val secondLabels =
      right(VectorField.fromValues(secondTarget.space, reversedLabels))
    val first = right(
      LabelAssignment.toPartialSurjection(
        bridge,
        image,
        firstTarget.space,
        firstLabels,
        permissivePolicy
      )
    )
    val second = right(
      LabelAssignment.toPartialSurjection(
        bridge,
        image,
        secondTarget.space,
        secondLabels,
        permissivePolicy
      )
    )

    assertNotEquals(firstRecord.key, secondRecord.key)
    assertEquals(
      first.toPartialMap.optionalTargetOrdinals,
      Vector(None, Some(0), Some(1), Some(0), None, Some(1))
    )
    assertEquals(
      second.toPartialMap.optionalTargetOrdinals,
      Vector(None, Some(1), Some(0), Some(1), None, Some(0))
    )
    assertEquals(
      right(first.equivalentUpToTargetRelabelingChecked(second)),
      true
    )

  test("persistent target identity follows numeric codes rather than storage dtype"):
    val id = domainId("parcels-storage-independent")
    val intRecord = right(
      LabelAssignment.canonicalTargetRecord(
        id,
        "integer parcels",
        labels
      )
    )
    val longLabels =
      labels.map: label =>
        LabelDefinition(
          label.value.toLong,
          label.name,
          label.metadata
        )
    val longRecord = right(
      LabelAssignment.canonicalTargetRecord(
        id,
        "long parcels",
        longLabels
      )
    )

    assertEquals(intRecord.key, longRecord.key)
    assertNotEquals(intRecord.metadata, longRecord.metadata)

  test("label catalogs reject ambiguous background and duplicate values"):
    val duplicate =
      labels :+ LabelDefinition(
        20,
        "duplicate twenty",
        ParcelMetadata("#ffffff", None)
      )
    assertEquals(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-duplicate"),
        "duplicate parcels",
        duplicate
      ),
      Left(LabelAssignmentError.DuplicateLabelValue(20L, 0, 2))
    )

    val grid = persistentGrid2("background-conflict", Vector(1, 2))
    val bridge = register(grid, "background source").value
    val image = categoricalImage(
      grid,
      Vector(Vector(20, 10)),
      "background source"
    )
    val record = right(
      LabelAssignment.canonicalTargetRecord(
        domainId("parcels-background-conflict"),
        "background-conflict parcels",
        labels
      )
    )
    val target = right(DomainRegistry.empty.restore(record))
    val labelField = right(VectorField.fromValues(target.space, labels))
    assertEquals(
      LabelAssignment.toPartialSurjection(
        bridge,
        image,
        target.space,
        labelField,
        LabelAssignmentPolicy(
          backgroundValues = Vector(20),
          unknownLabels = UnknownLabelPolicy.Reject
        )
      ),
      Left(LabelAssignmentError.BackgroundLabelConflict(20L, 0))
    )

  test("an explicitly all-background image admits an empty target domain"):
    val grid = persistentGrid2("empty-target", Vector(1, 3))
    val bridge = register(grid, "empty-target source").value
    val image = categoricalImage(
      grid,
      Vector(Vector(0, 0, 0)),
      "empty-target source"
    )
    val discovered = right(
      LabelAssignment.discover(
        bridge,
        image,
        domainId("parcels-empty"),
        "empty parcels",
        Vector.empty[LabelDefinition[Int, ParcelMetadata]],
        LabelAssignmentPolicy(
          backgroundValues = Vector(0),
          unknownLabels = UnknownLabelPolicy.Reject
        ),
        DomainRegistry.empty
      )
    )

    assertEquals(discovered.target.size, 0)
    assertEquals(discovered.labels.toVector, Vector.empty)
    assert(discovered.assignment.support.isEmpty)

  private def categoricalImage[F <: Frame[D2]](
      grid: Grid[F, D2],
      values: Vector[Vector[Int]],
      metadataLabel: String
  ) =
    imageRight(
      Sampled.categorical(
        grid,
        NonSpatialAxes.empty,
        NDArray.tabulate[Int](grid.shape(0), grid.shape(1)): (i, j) =>
          values(i)(j),
        ImageMetadata.named(metadataLabel)
      )
    )

  private def persistentGrid2(
      suffix: String,
      shape: Vector[Int]
  ): Grid[? <: Frame[D2], D2] =
    val frame = right(
      Frame.persistentNamed[D2](
        frameId(s"frame-$suffix"),
        s"frame $suffix",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )
    right(
      Grid.createPersistent(gridId(s"grid-$suffix"), frame)(
        shape,
        Affine.identity[D2]
      )
    )

  private def register[F <: Frame[D2]](
      grid: Grid[F, D2],
      name: String
  ): GridDomainResolution[F, D2] =
    right(GridDomain.register(grid, name, DomainRegistry.empty))

  private def domainId(value: String): DomainId =
    right(DomainId.parse(value))

  private def frameId(value: String): FrameId =
    right(FrameId.parse(value))

  private def gridId(value: String): GridId =
    right(GridId.parse(value))

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(s"expected Right, found Left($error)")

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    right(value)
