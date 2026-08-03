package image4s

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class AxisSuite extends ScalaCheckSuite:
  property("every coordinate representation derives extent and round trips"):
    val kinds =
      Vector(
        AxisKind.Time,
        AxisKind.Channel,
        AxisKind.Echo,
        AxisKind.Coil,
        AxisKind.Direction,
        AxisKind.Batch,
        AxisKind.Other,
        imageRight(AxisKind.custom("study:trial"))
      )
    val units =
      Vector(
        AxisUnit.Unitless,
        AxisUnit.Seconds,
        AxisUnit.Milliseconds,
        AxisUnit.Microseconds,
        AxisUnit.Hertz,
        AxisUnit.PartsPerMillion,
        AxisUnit.RadiansPerSecond,
        AxisUnit.Degrees,
        AxisUnit.Radians,
        imageRight(AxisUnit.custom("scanner:tick"))
      )
    forAll(
      Gen.choose(1, 64),
      Gen.choose(-1000.0, 1000.0),
      Gen
        .choose(-100.0, 100.0)
        .suchThat(step => step != 0.0 && step.isFinite),
      Gen.choose(0, kinds.size - 1),
      Gen.choose(0, units.size - 1)
    ): (extent, origin, step, kindIndex, unitIndex) =>
      val kind = kinds(kindIndex)
      val unit = units(unitIndex)
      val axes =
        Vector(
          imageRight(Axis.ordinal("ordinal", kind, extent)),
          imageRight(
            Axis.regular(
              "regular",
              kind,
              extent,
              origin,
              step,
              unit
            )
          ),
          imageRight(
            Axis.explicit(
              "explicit",
              kind,
              Vector.tabulate(extent)(index => origin + index.toDouble * step),
              unit
            )
          ),
          imageRight(
            Axis.categorical(
              "categorical",
              kind,
              Vector.tabulate(extent)(index => s"sample-$index")
            )
          )
        )

      axes.foreach { axis =>
        val restored = imageRight(Axis.fromRecord(axis.record))
        assertEquals(restored.record, axis.record)
        assertEquals(restored.extent, extent)
        assertEquals(
          (0 until extent).count(index => restored.coordinates(index).nonEmpty),
          extent
        )
        assertEquals(restored.coordinates(-1), None)
        assertEquals(restored.coordinates(extent), None)
      }

  property("regular coordinate lookup follows origin plus index times step"):
    forAll(
      Gen.choose(1, 128),
      Gen.choose(-10000.0, 10000.0),
      Gen
        .choose(-1000.0, 1000.0)
        .suchThat(step => step != 0.0 && step.isFinite),
      Gen.choose(0, 100000)
    ): (extent, origin, step, rawIndex) =>
      val index = rawIndex % extent
      val axis =
        imageRight(
          Axis.regular(
            name = "time",
            kind = AxisKind.Time,
            extent = extent,
            origin = origin,
            step = step,
            unit = AxisUnit.Seconds
          )
        )

      assertEquals(axis.extent, extent)
      assertEquals(
        axis.coordinateAt(index),
        Right(
          AxisCoordinate.Numeric(
            origin + index.toDouble * step,
            AxisUnit.Seconds
          )
        )
      )

  property("explicit coordinates retain every declared finite value"):
    forAll(
      Gen.choose(1, 64),
      Gen.choose(-1000.0, 1000.0),
      Gen.choose(0, 100000)
    ): (extent, base, rawIndex) =>
      val values =
        Vector.tabulate(extent)(index => base + index.toDouble * 0.375)
      val index = rawIndex % extent
      val axis =
        imageRight(
          Axis.explicit(
            "echo",
            AxisKind.Echo,
            values,
            AxisUnit.Milliseconds
          )
        )

      assertEquals(axis.extent, values.size)
      assertEquals(
        axis.coordinateAt(index),
        Right(
          AxisCoordinate.Numeric(
            values(index),
            AxisUnit.Milliseconds
          )
        )
      )

  property("categorical coordinates retain every declared label"):
    forAll(
      Gen.choose(1, 64),
      Gen.choose(0, 100000)
    ): (extent, rawIndex) =>
      val labels = Vector.tabulate(extent)(index => s"trial-$index")
      val index = rawIndex % extent
      val axis =
        imageRight(
          Axis.categorical(
            "trial",
            imageRight(AxisKind.custom("experiment:trial")),
            labels
          )
        )

      assertEquals(axis.extent, labels.size)
      assertEquals(
        axis.coordinateAt(index),
        Right(AxisCoordinate.Categorical(labels(index)))
      )

  property("axis record round trips preserve kind, units, and coordinates"):
    forAll(
      Gen.choose(1, 64),
      Gen.choose(-1000.0, 1000.0),
      Gen
        .choose(-100.0, 100.0)
        .suchThat(step => step != 0.0 && step.isFinite)
    ): (extent, origin, step) =>
      val kind = imageRight(AxisKind.custom("bids:frequency-bin"))
      val unit = imageRight(AxisUnit.custom("scanner:tick"))
      val source =
        imageRight(
          Axis.regular(
            "frequency",
            kind,
            extent,
            origin,
            step,
            unit
          )
        )
      val restored = imageRight(Axis.fromRecord(source.record))

      assert(restored ne source)
      assertEquals(restored.record, source.record)
      assert(restored.coordinates == source.coordinates)
      assertEquals(
        restored.coordinates.hashCode(),
        source.coordinates.hashCode()
      )

  property("axis permutations preserve coordinate records in declared order"):
    val permutations =
      Gen.oneOf(Vector(0, 1, 2).permutations.map(_.toVector).toVector)
    forAll(permutations): order =>
      val source =
        imageRight(
          NonSpatialAxes.from(
            Vector(
              imageRight(
                Axis.regular(
                  "time",
                  AxisKind.Time,
                  3,
                  0.0,
                  0.8,
                  AxisUnit.Seconds
                )
              ),
              imageRight(
                Axis.explicit(
                  "echo",
                  AxisKind.Echo,
                  Vector(12.0, 28.0),
                  AxisUnit.Milliseconds
                )
              ),
              imageRight(
                Axis.categorical(
                  "coil",
                  AxisKind.Coil,
                  Vector("head", "neck")
                )
              )
            )
          )
        )
      val permuted = imageRight(source.permute(order))

      assertEquals(
        permuted.records,
        order.map(source.records)
      )
      order.indices.foreach { target =>
        val sourceAxis = order(target)
        val coordinate =
          imageRight(source.coordinateAt(sourceAxis, 0))
        assertEquals(
          permuted.coordinateAt(target, 0),
          Right(coordinate)
        )
      }

  test("regular, explicit, categorical, and ordinal records round trip"):
    val customKind = imageRight(AxisKind.custom("study:trial"))
    val customUnit = imageRight(AxisUnit.custom("vendor:tick"))
    val axes =
      Vector(
        imageRight(Axis.ordinal("batch", AxisKind.Batch, 4)),
        imageRight(
          Axis.regular(
            "time",
            AxisKind.Time,
            3,
            0.0,
            800.0,
            AxisUnit.Milliseconds
          )
        ),
        imageRight(
          Axis.explicit(
            "frequency",
            AxisKind.Other,
            Vector(8.0, 13.0, 21.0),
            AxisUnit.Hertz
          )
        ),
        imageRight(
          Axis.categorical(
            "trial",
            customKind,
            Vector("left", "right", "catch")
          )
        ),
        imageRight(
          Axis.regular(
            "tick",
            AxisKind.Other,
            2,
            10.0,
            -0.5,
            customUnit
          )
        )
      )
    val records = axes.map(_.record)
    val restored = imageRight(NonSpatialAxes.fromRecords(records))

    assertEquals(restored.records, records)
    assertEquals(restored.shape, Vector(4, 3, 3, 3, 2))

  test("time sampling distinguishes TR and irregular schedules"):
    val fast =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          240,
          0.0,
          800.0,
          AxisUnit.Milliseconds
        )
      )
    val slow =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          240,
          0.0,
          2.0,
          AxisUnit.Seconds
        )
      )
    val irregular =
      imageRight(
        Axis.explicit(
          "time",
          AxisKind.Time,
          Vector.tabulate(240)(index => index.toDouble * 0.8 + (index % 3).toDouble * 0.01),
          AxisUnit.Seconds
        )
      )

    assertNotEquals(fast.record, slow.record)
    assertNotEquals(fast.record, irregular.record)
    assertNotEquals(slow.record, irregular.record)

  test("independently allocated coordinate payloads use content equality"):
    val firstExplicit =
      imageRight(
        Axis.explicit(
          "echo",
          AxisKind.Echo,
          Array(11.0, 22.0, 33.0).toVector,
          AxisUnit.Milliseconds
        )
      )
    val secondExplicit =
      imageRight(
        Axis.explicit(
          "echo",
          AxisKind.Echo,
          Array(11.0, 22.0, 33.0).toVector,
          AxisUnit.Milliseconds
        )
      )
    val firstCategorical =
      imageRight(
        Axis.categorical(
          "channel",
          AxisKind.Channel,
          Array("red", "green", "blue").toVector
        )
      )
    val secondCategorical =
      imageRight(
        Axis.categorical(
          "channel",
          AxisKind.Channel,
          Array("red", "green", "blue").toVector
        )
      )

    assert(firstExplicit ne secondExplicit)
    assert(firstExplicit.coordinates == secondExplicit.coordinates)
    assertEquals(
      firstExplicit.coordinates.hashCode(),
      secondExplicit.coordinates.hashCode()
    )
    assertEquals(firstExplicit.record, secondExplicit.record)

    assert(firstCategorical ne secondCategorical)
    assert(firstCategorical.coordinates == secondCategorical.coordinates)
    assertEquals(
      firstCategorical.coordinates.hashCode(),
      secondCategorical.coordinates.hashCode()
    )
    assertEquals(firstCategorical.record, secondCategorical.record)

  test("coordinate lookup and permutations report typed bounds failures"):
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          0.8,
          AxisUnit.Seconds
        )
      )
    val channel =
      imageRight(Axis.ordinal("channel", AxisKind.Channel, 3))
    val coil =
      imageRight(
        Axis.categorical(
          "coil",
          AxisKind.Coil,
          Vector("head", "neck")
        )
      )
    val axes =
      imageRight(NonSpatialAxes.from(Vector(time, channel, coil)))
    val removed = imageRight(axes.remove(1))

    assertEquals(
      time.coordinateAt(-1),
      Left(
        ImageError.NonSpatialIndexOutOfBounds(
          imageRight(AxisName.parse("time")),
          -1,
          2
        )
      )
    )
    assertEquals(
      axes.coordinateAt(3, 0),
      Left(ImageError.NonSpatialAxisOutOfBounds(3, 3))
    )
    assertEquals(
      removed.records,
      Vector(time.record, coil.record)
    )
    assertEquals(
      axes.permute(Vector(0, 1)),
      Left(ImageError.NonSpatialAxisPermutationRankMismatch(3, 2))
    )
    assertEquals(
      axes.permute(Vector(0, 0, 2)),
      Left(
        ImageError.InvalidNonSpatialAxisPermutation(
          Vector(0, 0, 2),
          3
        )
      )
    )
    assertEquals(
      axes.permute(Vector(0, 1, 3)),
      Left(
        ImageError.InvalidNonSpatialAxisPermutation(
          Vector(0, 1, 3),
          3
        )
      )
    )

  test("malformed coordinate sampling and custom identifiers fail closed"):
    assertEquals(
      Axis.regular(
        "time",
        AxisKind.Time,
        0,
        0.0,
        1.0,
        AxisUnit.Seconds
      ),
      Left(ImageError.NonPositiveAxisExtent("time", 0))
    )
    Axis.regular(
      "time",
      AxisKind.Time,
      2,
      Double.NaN,
      1.0,
      AxisUnit.Seconds
    ) match
      case Left(ImageError.NonFiniteAxisOrigin("time", value)) =>
        assert(value.isNaN)
      case other =>
        fail(s"expected NonFiniteAxisOrigin with NaN, got $other")
    Vector(0.0, Double.PositiveInfinity).foreach { step =>
      assertEquals(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          step,
          AxisUnit.Seconds
        ),
        Left(ImageError.InvalidAxisStep("time", step))
      )
    }
    Axis.regular(
      "time",
      AxisKind.Time,
      2,
      0.0,
      Double.NaN,
      AxisUnit.Seconds
    ) match
      case Left(ImageError.InvalidAxisStep("time", value)) =>
        assert(value.isNaN)
      case other =>
        fail(s"expected InvalidAxisStep with NaN, got $other")
    assertEquals(
      Axis.explicit(
        "echo",
        AxisKind.Echo,
        Vector.empty,
        AxisUnit.Milliseconds
      ),
      Left(ImageError.NonPositiveAxisExtent("echo", 0))
    )
    assertEquals(
      Axis.explicit(
        "echo",
        AxisKind.Echo,
        Vector(1.0, Double.NegativeInfinity),
        AxisUnit.Milliseconds
      ),
      Left(
        ImageError.NonFiniteAxisCoordinate(
          "echo",
          1,
          Double.NegativeInfinity
        )
      )
    )
    assertEquals(
      Axis.categorical(
        "trial",
        AxisKind.Other,
        Vector.empty
      ),
      Left(ImageError.NonPositiveAxisExtent("trial", 0))
    )
    assertEquals(
      Axis.categorical(
        "trial",
        AxisKind.Other,
        Vector("valid", " invalid")
      ),
      Left(
        ImageError.InvalidCategoricalAxisLabel(
          "trial",
          1,
          " invalid"
        )
      )
    )
    assertEquals(
      AxisKind.custom("time"),
      Left(ImageError.ReservedAxisKindId("time"))
    )
    assertEquals(
      AxisKind.custom("Not Valid"),
      Left(ImageError.InvalidAxisKindId("Not Valid"))
    )
    assertEquals(
      AxisUnit.custom("ms"),
      Left(ImageError.ReservedAxisUnitId("ms"))
    )
    assertEquals(
      AxisUnit.custom(" not-valid"),
      Left(ImageError.InvalidAxisUnitId(" not-valid"))
    )

  test("axis records are neutral untrusted input and restore validates them"):
    assertEquals(
      Axis.fromRecord(
        AxisRecord(
          "time",
          "time",
          AxisCoordinatesRecord.Regular(
            3,
            0.0,
            0.0,
            "s"
          )
        )
      ),
      Left(ImageError.InvalidAxisStep("time", 0.0))
    )
    assertEquals(
      Axis.fromRecord(
        AxisRecord(
          "trial",
          "study:trial",
          AxisCoordinatesRecord.Categorical(
            Vector("left", "")
          )
        )
      ),
      Left(
        ImageError.InvalidCategoricalAxisLabel(
          "trial",
          1,
          ""
        )
      )
    )
    assertEquals(
      Axis.fromRecord(
        AxisRecord(
          "frequency",
          "Invalid Kind",
          AxisCoordinatesRecord.Ordinal(3)
        )
      ),
      Left(ImageError.InvalidAxisKindId("Invalid Kind"))
    )
    assertEquals(
      Axis.fromRecord(
        AxisRecord(
          "time",
          "time",
          AxisCoordinatesRecord.Regular(
            3,
            0.0,
            0.8,
            "Invalid Unit"
          )
        )
      ),
      Left(ImageError.InvalidAxisUnitId("Invalid Unit"))
    )

  test("the compatibility constructor remains explicitly ordinal"):
    val axis = imageRight(Axis.create("time", 3, AxisKind.Time))

    assertEquals(
      axis.record,
      AxisRecord(
        "time",
        "time",
        AxisCoordinatesRecord.Ordinal(3)
      )
    )
    assertEquals(
      axis.coordinateAt(2),
      Right(AxisCoordinate.Ordinal(2))
    )

  test("shared record fixture has a stable canonical checksum"):
    val records =
      Vector(
        imageRight(
          Axis.regular(
            "time",
            AxisKind.Time,
            3,
            0.0,
            0.8,
            AxisUnit.Seconds
          )
        ).record,
        imageRight(
          Axis.explicit(
            "echo",
            AxisKind.Echo,
            Vector(12.0, 28.0, 44.0),
            AxisUnit.Milliseconds
          )
        ).record,
        imageRight(
          Axis.categorical(
            "coil",
            AxisKind.Coil,
            Vector("head", "neck")
          )
        ).record,
        imageRight(
          Axis.ordinal("trial", AxisKind.Other, 2)
        ).record
      )
    val encoded = records.map(encodeRecord).mkString("\n")

    assertEquals(
      encoded,
      "time|time|regular|3|0000000000000000|3fe999999999999a|s\n" +
        "echo|echo|explicit|" +
        "4028000000000000,403c000000000000,4046000000000000|ms\n" +
        "coil|coil|categorical|head,neck\n" +
        "trial|other|ordinal|2"
    )
    assertEquals(fnv1a32(encoded), 1580019158L)

  private def encodeRecord(record: AxisRecord): String =
    val prefix = s"${record.name}|${record.kind}"
    record.coordinates match
      case AxisCoordinatesRecord.Ordinal(extent) =>
        s"$prefix|ordinal|$extent"
      case AxisCoordinatesRecord.Regular(extent, origin, step, unit) =>
        s"$prefix|regular|$extent|${encodeDouble(origin)}|" +
          s"${encodeDouble(step)}|$unit"
      case AxisCoordinatesRecord.Explicit(values, unit) =>
        s"$prefix|explicit|${values.map(encodeDouble).mkString(",")}|$unit"
      case AxisCoordinatesRecord.Categorical(labels) =>
        s"$prefix|categorical|${labels.mkString(",")}"

  private def encodeDouble(value: Double): String =
    val unpadded =
      java.lang.Long.toHexString(
        java.lang.Double.doubleToRawLongBits(value)
      )
    "0" * (16 - unpadded.length) + unpadded

  private def fnv1a32(value: String): Long =
    var hash = 0x811c9dc5L
    var index = 0
    while index < value.length do
      hash ^= value.charAt(index).toLong
      hash = (hash * 0x01000193L) & 0xffffffffL
      index += 1
    hash

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
