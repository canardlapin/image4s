package image4s.laws

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.Axis
import image4s.AxisKind
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.apply
import munit.FunSuite
import ravel.CanonicalArray
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid

final class ImageRepresentationPerformanceSuite extends FunSuite:
  test(
    "Sampled aliases add no material allocation tax versus raw NDArray access"
  ):
    val nx = 48
    val ny = 37
    val cases =
      Vector(
        dtypeCase("Byte")(
          NDArray.tabulate[Byte](nx, ny)((i, j) => (i + 3 * j).toByte),
          data => imageRight(Sampled.categorical(plane(nx, ny), NonSpatialAxes.empty, data)),
          (array, i, j) => array(i, j).toLong,
          (image, i, j) => image(i, j).toLong
        ),
        dtypeCase("Short")(
          NDArray.tabulate[Short](nx, ny)((i, j) => (i + 3 * j).toShort),
          data => imageRight(Sampled.categorical(plane(nx, ny), NonSpatialAxes.empty, data)),
          (array, i, j) => array(i, j).toLong,
          (image, i, j) => image(i, j).toLong
        ),
        dtypeCase("Float")(
          NDArray.tabulate[Float](nx, ny)((i, j) => (i + 3 * j).toFloat),
          data => imageRight(Sampled.continuous(plane(nx, ny), NonSpatialAxes.empty, data)),
          (array, i, j) => java.lang.Float.floatToIntBits(array(i, j)).toLong,
          (image, i, j) => java.lang.Float.floatToIntBits(image(i, j)).toLong
        ),
        dtypeCase("Double")(
          NDArray.tabulate[Double](nx, ny)((i, j) => (i + 3 * j).toDouble),
          data => imageRight(Sampled.continuous(plane(nx, ny), NonSpatialAxes.empty, data)),
          (array, i, j) => java.lang.Double.doubleToLongBits(array(i, j)),
          (image, i, j) => java.lang.Double.doubleToLongBits(image(i, j))
        )
      )

    cases.foreach { dtypeCase =>
      var warmup = 0
      while warmup < 50 do
        dtypeCase.ravelChecksum()
        dtypeCase.sampledChecksum()
        warmup += 1

      val ravelReceipt = measureChecksum(dtypeCase.name + "-ravel", dtypeCase.ravelChecksum)
      val sampledReceipt =
        measureChecksum(dtypeCase.name + "-sampled", dtypeCase.sampledChecksum)

      assertEquals(
        sampledReceipt.checksum,
        ravelReceipt.checksum,
        s"${dtypeCase.name}: Sampled and NDArray traversals disagree"
      )
      assert(
        sampledReceipt.allocatedBytes <= 64L * 1024L,
        s"${dtypeCase.name} Sampled traversal allocated " +
          s"${sampledReceipt.allocatedBytes} bytes"
      )
      assert(
        ravelReceipt.allocatedBytes <= 64L * 1024L,
        s"${dtypeCase.name} NDArray traversal allocated " +
          s"${ravelReceipt.allocatedBytes} bytes"
      )
      // Alias tax gate: Sampled ranked access may not be dramatically slower
      // than the underlying Ravel array on the same layout.
      assert(
        sampledReceipt.medianNanos <= ravelReceipt.medianNanos * 4L + 50_000L,
        s"${dtypeCase.name}: Sampled took ${sampledReceipt.medianNanos} ns " +
          s"versus ${ravelReceipt.medianNanos} ns for NDArray"
      )
      println(
        s"DENSE-GENERIC JVM baseline: dtype=${dtypeCase.name}, " +
          s"ravelAllocated=${ravelReceipt.allocatedBytes} B, " +
          s"sampledAllocated=${sampledReceipt.allocatedBytes} B, " +
          s"ravelMedian=${ravelReceipt.medianNanos} ns, " +
          s"sampledMedian=${sampledReceipt.medianNanos} ns"
      )
    }

  test("matched image access workloads record checksums and allocation"):
    val fixture = new Fixture(24, 17, 11, 5)
    val workloads =
      Vector(
        Workload("legacy-c-order", fixture.samples, fixture.legacyCOrder),
        Workload("ravel-c-order", fixture.samples, fixture.ravelCOrder),
        Workload(
          "sampled-ranked-c-order",
          fixture.samples,
          fixture.sampledRankedCOrder
        ),
        Workload(
          "sampled-checked-c-order",
          fixture.samples,
          fixture.sampledCheckedCOrder
        ),
        Workload(
          "primitive-canonical-linear",
          fixture.samples,
          fixture.primitiveCanonicalLinear
        ),
        Workload(
          "ravel-canonical-linear",
          fixture.samples,
          fixture.ravelCanonicalLinear
        ),
        Workload(
          "legacy-volume-order",
          fixture.samples,
          fixture.legacyVolumeOrder
        ),
        Workload(
          "ravel-volume-order",
          fixture.samples,
          fixture.ravelVolumeOrder
        ),
        Workload(
          "legacy-z-slices",
          fixture.samples,
          fixture.legacyZSlices
        ),
        Workload(
          "ravel-z-slices",
          fixture.samples,
          fixture.ravelZSlices
        )
      )

    workloads.foreach(workload =>
      var warmup = 0
      while warmup < 100 do
        workload.run()
        warmup += 1
    )

    val receipts = workloads.map(measure)
    val expected = receipts.head.signature
    receipts.foreach(receipt =>
      assertEquals(
        receipt.signature,
        expected,
        s"${receipt.name} changed the matched logical signature"
      )
    )

    val directRavel =
      receipts.find(_.name == "ravel-c-order").getOrElse(fail("missing row"))
    val rankedSampled =
      receipts
        .find(_.name == "sampled-ranked-c-order")
        .getOrElse(fail("missing row"))
    val primitiveCanonical =
      receipts
        .find(_.name == "primitive-canonical-linear")
        .getOrElse(fail("missing row"))
    val ravelCanonical =
      receipts
        .find(_.name == "ravel-canonical-linear")
        .getOrElse(fail("missing row"))
    assert(
      directRavel.allocatedBytes <= 64L * 1024L,
      s"direct ranked Ravel traversal allocated " +
        s"${directRavel.allocatedBytes} bytes"
    )
    assert(
      rankedSampled.allocatedBytes <= 64L * 1024L,
      s"ranked Sampled traversal allocated " +
        s"${rankedSampled.allocatedBytes} bytes"
    )
    assert(
      ravelCanonical.allocatedBytes <= 64L * 1024L,
      s"canonical Ravel traversal allocated " +
        s"${ravelCanonical.allocatedBytes} bytes"
    )
    assert(
      ravelCanonical.medianNanos <= primitiveCanonical.medianNanos * 3L,
      s"canonical Ravel traversal took ${ravelCanonical.medianNanos} ns " +
        s"versus ${primitiveCanonical.medianNanos} ns for the matched " +
        "primitive oracle"
    )

    receipts.foreach { receipt =>
      val nanosPerSample =
        receipt.medianNanos.toDouble / receipt.samples.toDouble
      val revision =
        sys.props.getOrElse("image4s.revision", "unspecified")
      println(
        f"IMG-CONTRACT JVM baseline: workload=${receipt.name}, " +
          f"samples=${receipt.samples}%d, " +
          f"allocated=${receipt.allocatedBytes}%d B, " +
          f"median=${receipt.medianNanos}%d ns, " +
          f"nsPerSample=$nanosPerSample%.3f, " +
          f"sum=${receipt.signature.sum}%.6f, " +
          f"weightedSum=${receipt.signature.weightedSum}%.6f, " +
          s"ravel=52cea553b8b0e8f57ce000f32167ad56436da8a0, " +
          s"revision=$revision"
      )
    }

  private final case class Signature(
      samples: Int,
      sum: Double,
      weightedSum: Double
  )

  private final case class Workload(
      name: String,
      samples: Int,
      run: () => Signature
  )

  private final case class Receipt(
      name: String,
      samples: Int,
      allocatedBytes: Long,
      medianNanos: Long,
      signature: Signature
  )

  private final class Fixture(
      nx: Int,
      ny: Int,
      nz: Int,
      nt: Int
  ):
    val samples: Int =
      nx * ny * nz * nt

    private val legacy =
      val values = new Array[Double](samples)
      var t = 0
      while t < nt do
        var k = 0
        while k < nz do
          var j = 0
          while j < ny do
            var i = 0
            while i < nx do
              values(legacyIndex(i, j, k, t)) = value(i, j, k, t)
              i += 1
            j += 1
          k += 1
        t += 1
      values

    private val primitiveCanonical =
      val values = new Array[Double](samples)
      var i = 0
      while i < nx do
        var j = 0
        while j < ny do
          var k = 0
          while k < nz do
            var t = 0
            while t < nt do
              values(cIndex(i, j, k, t)) =
                legacy(legacyIndex(i, j, k, t))
              t += 1
            k += 1
          j += 1
        i += 1
      values

    private val canonical =
      NDArray.tabulate[Double](nx, ny, nz, nt)((i, j, k, t) =>
        legacy(legacyIndex(i, j, k, t))
      )
    private val canonicalAccess =
      CanonicalArray.require(canonical)

    private val frame =
      geometryRight(Frame.named[D3]("image-contract-performance"))
    private val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(nx, ny, nz),
          Affine.identity[D3]
        )
      )
    private val time =
      imageRight(Axis.create("time", nt, AxisKind.Time))
    private val axes =
      imageRight(NonSpatialAxes.from(Vector(time)))
    private val sampled =
      imageRight(Sampled.continuous(grid, axes, canonical))

    val legacyCOrder: () => Signature =
      () => traverseCOrder((i, j, k, t) => legacy(legacyIndex(i, j, k, t)))

    val ravelCOrder: () => Signature =
      () => traverseCOrder(canonical.apply)

    val sampledRankedCOrder: () => Signature =
      () => traverseCOrder(sampled.apply)

    val sampledCheckedCOrder: () => Signature =
      () =>
        traverseCOrder((i, j, k, t) =>
          sampled.valueAt(Vector(i, j, k), Vector(t)) match
            case Right(result) => result
            case Left(error)   => fail(error.message)
        )

    val primitiveCanonicalLinear: () => Signature =
      () => traverseCanonicalLinear(primitiveCanonical.apply)

    val ravelCanonicalLinear: () => Signature =
      () => traverseCanonicalLinear(canonicalAccess.readLinear)

    val legacyVolumeOrder: () => Signature =
      () =>
        traverseVolumeOrder((i, j, k, t) =>
          legacy(legacyIndex(i, j, k, t))
        )

    val ravelVolumeOrder: () => Signature =
      () => traverseVolumeOrder(canonical.apply)

    val legacyZSlices: () => Signature =
      () =>
        traverseZSlices((i, j, k, t) =>
          legacy(legacyIndex(i, j, k, t))
        )

    val ravelZSlices: () => Signature =
      () => traverseZSlices(canonical.apply)

    private inline def traverseCOrder(
        inline read: (Int, Int, Int, Int) => Double
    ): Signature =
      var sum = 0.0
      var weighted = 0.0
      var visited = 0
      var i = 0
      while i < nx do
        var j = 0
        while j < ny do
          var k = 0
          while k < nz do
            var t = 0
            while t < nt do
              val sample = read(i, j, k, t)
              sum += sample
              weighted += sample * (cIndex(i, j, k, t) + 1).toDouble
              visited += 1
              t += 1
            k += 1
          j += 1
        i += 1
      Signature(visited, sum, weighted)

    private inline def traverseVolumeOrder(
        inline read: (Int, Int, Int, Int) => Double
    ): Signature =
      var sum = 0.0
      var weighted = 0.0
      var visited = 0
      var t = 0
      while t < nt do
        var k = 0
        while k < nz do
          var j = 0
          while j < ny do
            var i = 0
            while i < nx do
              val sample = read(i, j, k, t)
              sum += sample
              weighted += sample * (cIndex(i, j, k, t) + 1).toDouble
              visited += 1
              i += 1
            j += 1
          k += 1
        t += 1
      Signature(visited, sum, weighted)

    private inline def traverseCanonicalLinear(
        inline read: Int => Double
    ): Signature =
      var sum = 0.0
      var weighted = 0.0
      var index = 0
      while index < samples do
        val sample = read(index)
        sum += sample
        weighted += sample * (index + 1).toDouble
        index += 1
      Signature(index, sum, weighted)

    private inline def traverseZSlices(
        inline read: (Int, Int, Int, Int) => Double
    ): Signature =
      var sum = 0.0
      var weighted = 0.0
      var visited = 0
      var k = 0
      while k < nz do
        var t = 0
        while t < nt do
          var j = 0
          while j < ny do
            var i = 0
            while i < nx do
              val sample = read(i, j, k, t)
              sum += sample
              weighted += sample * (cIndex(i, j, k, t) + 1).toDouble
              visited += 1
              i += 1
            j += 1
          t += 1
        k += 1
      Signature(visited, sum, weighted)

    private def legacyIndex(i: Int, j: Int, k: Int, t: Int): Int =
      i + nx * (j + ny * (k + nz * t))

    private def cIndex(i: Int, j: Int, k: Int, t: Int): Int =
      t + nt * (k + nz * (j + ny * i))

    private def value(i: Int, j: Int, k: Int, t: Int): Double =
      i.toDouble +
        10.0 * j.toDouble +
        100.0 * k.toDouble +
        1000.0 * t.toDouble

  private final case class DtypeCase(
      name: String,
      ravelChecksum: () => Long,
      sampledChecksum: () => Long
  )

  private final case class ChecksumReceipt(
      name: String,
      checksum: Long,
      allocatedBytes: Long,
      medianNanos: Long
  )

  private def dtypeCase[A, Img](
      name: String
  )(
      data: NDArray[A, Rank[2]],
      wrap: NDArray[A, Rank[2]] => Img,
      readArray: (NDArray[A, Rank[2]], Int, Int) => Long,
      readImage: (Img, Int, Int) => Long
  ): DtypeCase =
    val image = wrap(data)
    val nx = data.shape(0)
    val ny = data.shape(1)
    def checksum(read: (Int, Int) => Long): Long =
      var acc = 0L
      var i = 0
      while i < nx do
        var j = 0
        while j < ny do
          acc += read(i, j) * (1L + i + 31L * j)
          j += 1
        i += 1
      acc
    DtypeCase(
      name,
      () => checksum((i, j) => readArray(data, i, j)),
      () => checksum((i, j) => readImage(image, i, j))
    )

  private def plane(nx: Int, ny: Int) =
    val frame = geometryRight(Frame.named[D2]("dense-generic-perf"))
    geometryRight(Grid.in(frame)(Vector(nx, ny), Affine.identity[D2]))

  private def measureChecksum(
      name: String,
      run: () => Long
  ): ChecksumReceipt =
    val allocationSamples =
      Vector.tabulate(7)(_ => measuredAllocation(run()))
    val checksum = allocationSamples.head._1
    allocationSamples.tail.foreach(sample =>
      assertEquals(
        sample._1,
        checksum,
        s"$name changed checksum during allocation sampling"
      )
    )
    val allocatedBytes =
      allocationSamples.map(_._2).sorted.apply(allocationSamples.size / 2)
    val timings =
      Vector.tabulate(21) { _ =>
        val started = System.nanoTime()
        run()
        System.nanoTime() - started
      }.sorted
    ChecksumReceipt(
      name,
      checksum,
      allocatedBytes,
      timings(timings.size / 2)
    )

  private def measure(workload: Workload): Receipt =
    val allocationSamples =
      Vector.tabulate(7)(_ => measuredAllocation(workload.run()))
    val signature = allocationSamples.head._1
    allocationSamples.tail.foreach(sample =>
      assertEquals(
        sample._1,
        signature,
        s"${workload.name} changed signature during allocation sampling"
      )
    )
    val allocatedBytes =
      allocationSamples.map(_._2).sorted.apply(allocationSamples.size / 2)
    val timings =
      Vector.tabulate(21) { _ =>
        val started = System.nanoTime()
        workload.run()
        System.nanoTime() - started
      }.sorted
    Receipt(
      workload.name,
      workload.samples,
      allocatedBytes,
      timings(timings.size / 2),
      signature
    )

  private def measuredAllocation[A](value: => A): (A, Long) =
    val allocationBean =
      ManagementFactory.getThreadMXBean match
        case bean: ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
          if !bean.isThreadAllocatedMemoryEnabled then
            bean.setThreadAllocatedMemoryEnabled(true)
          bean
        case _ =>
          fail("this JVM does not expose per-thread allocation accounting")
    val threadId = Thread.currentThread().threadId()
    val before = allocationBean.getThreadAllocatedBytes(threadId)
    val result = value
    val allocated =
      allocationBean.getThreadAllocatedBytes(threadId) - before
    result -> allocated

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](
      value: Either[ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
