package image4s.nifti

import java.lang.management.ManagementFactory
import java.nio.file.Files
import java.nio.file.Path

import com.sun.management.ThreadMXBean
import image4s.Axis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.SomeSampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class NiftiProductionJvmSuite extends FunSuite:
  @volatile private var retained: AnyRef = new Object

  test("representative reads and writes have allocation-bounded receipts"):
    val directory = Files.createTempDirectory("image4s-nifti-production-")
    val volume = continuous3d(Vector(32, 28, 16))
    val fmri = continuous4d(Vector(32, 28, 16), 6)
    val labels = categorical3d(Vector(32, 28, 16))
    val floatOptions =
      NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)
    val labelOptions =
      NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
    val volumePath = directory.resolve("volume.nii")
    val fmriPath = directory.resolve("fmri.nii")
    val labelsPath = directory.resolve("labels.nii")
    val gzipPath = directory.resolve("fmri.nii.gz")

    niftiRight(Nifti.writeScalar(volumePath, volume, floatOptions))
    niftiRight(Nifti.writeScalar(fmriPath, fmri, floatOptions))
    niftiRight(Nifti.writeLabels(labelsPath, labels, labelOptions))
    niftiRight(Nifti.writeScalar(gzipPath, fmri, floatOptions))

    assertEquals(
      Nifti.ioStrategy(volumePath),
      NiftiIoStrategy.BoundedStreaming
    )
    assertEquals(
      Nifti.ioStrategy(gzipPath),
      NiftiIoStrategy.BoundedStreaming
    )

    val volumeDecoded =
      niftiRight(Nifti.readScaledFloat(volumePath)).image
    val fmriDecoded =
      niftiRight(Nifti.readScaledFloat(fmriPath)).image
    val labelsDecoded =
      niftiRight(Nifti.readLabels(labelsPath)).image
    val gzipDecoded =
      niftiRight(Nifti.readScaledFloat(gzipPath)).image

    assertEquals(
      checksumFloat(volumeDecoded),
      checksumDouble(volume.data.elementsIterator)
    )
    assertEquals(
      checksumFloat(fmriDecoded),
      checksumDouble(fmri.data.elementsIterator)
    )
    assertEquals(
      checksumLong(labelsDecoded),
      checksumLongValues(labels.data.elementsIterator)
    )
    assertEquals(checksumFloat(gzipDecoded), checksumFloat(fmriDecoded))

    val readCases =
      Vector(
        ReadCase(
          "float32-3d",
          volume.data.size,
          4,
          () => retainFloat(Nifti.readScaledFloat(volumePath))
        ),
        ReadCase(
          "float32-4d",
          fmri.data.size,
          4,
          () => retainFloat(Nifti.readScaledFloat(fmriPath))
        ),
        ReadCase(
          "int16-labels",
          labels.data.size,
          8,
          () => retainLong(Nifti.readLabels(labelsPath))
        ),
        ReadCase(
          "float32-4d-gzip",
          fmri.data.size,
          4,
          () => retainFloat(Nifti.readScaledFloat(gzipPath))
        )
      )

    readCases.foreach { readCase =>
      Vector.fill(3)(readCase.run())
      val samples =
        Vector.fill(7)(allocatedBytes(readCase.run())).sorted
      val median = samples(samples.size / 2)
      val finalBytes =
        readCase.samples.toLong * readCase.outputBytes.toLong
      val limit =
        finalBytes +
          readCase.samples.toLong * 40L +
          1024L * 1024L
      assert(
        median <= limit,
        s"${readCase.name} allocated $median bytes for " +
          s"${readCase.samples} values; limit=$limit"
      )
      receipt("read", readCase.name, readCase.samples, median)
    }

    val writeTarget = directory.resolve("fmri-write-court.nii")
    val writeRun = () =>
      retainFiles(
        Nifti.writeScalar(writeTarget, fmri, floatOptions)
      )
    Vector.fill(3)(writeRun())
    val writeSamples =
      Vector.fill(7)(allocatedBytes(writeRun())).sorted
    val writeMedian = writeSamples(writeSamples.size / 2)
    val writeLimit =
      fmri.data.size.toLong * 64L + 1024L * 1024L
    assert(
      writeMedian <= writeLimit,
      s"Float32 4D write allocated $writeMedian bytes; limit=$writeLimit"
    )
    receipt("write", "float32-4d", fmri.data.size, writeMedian)

    val gzipWriteTarget =
      directory.resolve("fmri-write-court.nii.gz")
    val gzipWriteRun = () =>
      retainFiles(
        Nifti.writeScalar(gzipWriteTarget, fmri, floatOptions)
      )
    Vector.fill(3)(gzipWriteRun())
    val gzipWriteSamples =
      Vector.fill(7)(allocatedBytes(gzipWriteRun())).sorted
    val gzipWriteMedian =
      gzipWriteSamples(gzipWriteSamples.size / 2)
    val gzipWriteLimit =
      fmri.data.size.toLong * 72L + 2L * 1024L * 1024L
    assert(
      gzipWriteMedian <= gzipWriteLimit,
      s"gzip Float32 4D write allocated $gzipWriteMedian bytes; " +
        s"limit=$gzipWriteLimit"
    )
    receipt(
      "write",
      "float32-4d-gzip",
      fmri.data.size,
      gzipWriteMedian
    )

  test("stored UInt8 and Int16 reads stay below Double-path allocation"):
    val directory = Files.createTempDirectory("image4s-nifti-native-stored-")
    val shape = Vector(128, 128, 64)
    val uint8 = uint8Image(shape)
    val labels = categorical3d(shape)
    val uint8Path = directory.resolve("uint8.nii")
    val int16Path = directory.resolve("int16.nii")

    niftiRight(
      Nifti.writeScalar(
        uint8Path,
        uint8,
        NiftiWriteOptions.forDatatype(NiftiDatatype.UInt8)
      )
    )
    niftiRight(
      Nifti.writeLabels(
        int16Path,
        labels,
        NiftiWriteOptions.forDatatype(NiftiDatatype.Int16)
      )
    )

    val cases =
      Vector(
        (
          "uint8",
          uint8Path,
          () => retainStored(Nifti.readScalarStored(uint8Path))
        ),
        (
          "int16",
          int16Path,
          () => retainNativeLabels(Nifti.readLabelsNative(int16Path))
        )
      )
    cases.foreach { case (name, path, readStored) =>
      val readDouble = () => retained = niftiRight(Nifti.readScaledDouble(path)).image
      readStored()
      readDouble()
      val storedAllocations =
        Vector.fill(7)(allocatedBytes(readStored())).sorted
      val doubleAllocations =
        Vector.fill(7)(allocatedBytes(readDouble())).sorted
      val storedMedian = storedAllocations(storedAllocations.size / 2)
      val doubleMedian = doubleAllocations(doubleAllocations.size / 2)
      val storedNanos = Vector.fill(7)(elapsedNanos(readStored())).sorted
      val doubleNanos = Vector.fill(7)(elapsedNanos(readDouble())).sorted
      val valueCount = shape.product
      val limit = valueCount.toLong * 5L + 1024L * 1024L
      assert(
        storedMedian <= limit,
        s"native stored $name read allocated $storedMedian bytes; limit=$limit"
      )
      assert(
        storedMedian < doubleMedian,
        s"native stored $name read allocated $storedMedian bytes; " +
          s"Double materialization allocated $doubleMedian bytes"
      )
      receipt("read-stored", name, valueCount, storedMedian)
      receipt("read-scaled-double", name, valueCount, doubleMedian)
      timingReceipt(
        "read-stored",
        name,
        valueCount,
        storedNanos(storedNanos.size / 2)
      )
      timingReceipt(
        "read-scaled-double",
        name,
        valueCount,
        doubleNanos(doubleNanos.size / 2)
      )
    }

  private final case class ReadCase(
      name: String,
      samples: Int,
      outputBytes: Int,
      run: () => Unit
  )

  private final case class Signature(
      count: Int,
      sum: Double,
      weightedSum: Double
  )

  private def checksumFloat(
      sampled: SomeSampled[Float, image4s.Continuous]
  ): Signature =
    checksumDouble(
      sampled.value.data.elementsIterator.map(_.toDouble)
    )

  private def checksumLong(
      sampled: SomeSampled[Long, image4s.Categorical]
  ): Signature =
    checksumDouble(
      sampled.value.data.elementsIterator.map(_.toDouble)
    )

  private def checksumLongValues(
      values: Iterator[Long]
  ): Signature =
    checksumDouble(values.map(_.toDouble))

  private def checksumDouble(
      values: Iterator[Double]
  ): Signature =
    var count = 0
    var sum = 0.0
    var weighted = 0.0
    while values.hasNext do
      val value = values.next()
      sum += value
      weighted += value * (count.toDouble + 1.0)
      count += 1
    Signature(count, sum, weighted)

  private def retainFloat(
      decoded: Either[
        NiftiError,
        DecodedNifti[
          SomeSampled[Float, image4s.Continuous]
        ]
      ]
  ): Unit =
    retained = niftiRight(decoded).image.value.data

  private def retainLong(
      decoded: Either[
        NiftiError,
        DecodedNifti[
          SomeSampled[Long, image4s.Categorical]
        ]
      ]
  ): Unit =
    retained = niftiRight(decoded).image.value.data

  private def retainStored(
      decoded: Either[NiftiError, DecodedNifti[NiftiScalarStored]]
  ): Unit =
    retained = niftiRight(decoded).image

  private def retainNativeLabels(
      decoded: Either[NiftiError, DecodedNifti[NiftiLabelStored]]
  ): Unit =
    retained = niftiRight(decoded).image

  private def retainFiles(
      written: Either[NiftiError, NiftiFiles[Path]]
  ): Unit =
    retained = niftiRight(written)

  private def allocatedBytes(
      body: => Unit
  ): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("this JVM does not expose per-thread allocation accounting")
    val threadId = Thread.currentThread().threadId()
    val before = bean.getThreadAllocatedBytes(threadId)
    body
    val after = bean.getThreadAllocatedBytes(threadId)
    assert(retained ne null)
    after - before

  private def elapsedNanos(
      body: => Unit
  ): Long =
    val before = System.nanoTime()
    body
    System.nanoTime() - before

  private def receipt(
      operation: String,
      name: String,
      samples: Int,
      allocated: Long
  ): Unit =
    val revision =
      sys.props.getOrElse("image4s.revision", "unspecified")
    println(
      s"IMG-NIFTI JVM allocation: operation=$operation, case=$name, " +
        s"samples=$samples, allocated=$allocated B, " +
        s"java=${sys.props.getOrElse("java.version", "unknown")}, " +
        s"scala=${util.Properties.versionNumberString}, revision=$revision"
    )

  private def timingReceipt(
      operation: String,
      name: String,
      samples: Int,
      elapsedNanos: Long
  ): Unit =
    val revision =
      sys.props.getOrElse("image4s.revision", "unspecified")
    println(
      s"IMG-NIFTI JVM time: operation=$operation, case=$name, " +
        s"samples=$samples, elapsed=$elapsedNanos ns, " +
        s"java=${sys.props.getOrElse("java.version", "unknown")}, " +
        s"scala=${util.Properties.versionNumberString}, revision=$revision"
    )

  private def continuous3d(
      shape: Vector[Int]
  ) =
    val grid = gridFor(shape)
    imageRight(
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.tabulate[Double](shape(0), shape(1), shape(2)) { (i, j, k) =>
          (i + 17 * j + 101 * k).toDouble
        }
      )
    )

  private def uint8Image(
      shape: Vector[Int]
  ) =
    val grid = gridFor(shape)
    imageRight(
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.tabulate[Double](shape(0), shape(1), shape(2)) { (i, j, k) =>
          ((i + 17 * j + 101 * k) % 251).toDouble
        }
      )
    )

  private def continuous4d(
      shape: Vector[Int],
      timePoints: Int
  ) =
    val grid = gridFor(shape)
    val time =
      imageRight(Axis.create("time", timePoints, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    imageRight(
      Sampled.continuous(
        grid,
        axes,
        NDArray.tabulate[Double](
          shape(0),
          shape(1),
          shape(2),
          timePoints
        ) { (i, j, k, t) =>
          (i + 17 * j + 101 * k + 1009 * t).toDouble
        }
      )
    )

  private def categorical3d(
      shape: Vector[Int]
  ) =
    val grid = gridFor(shape)
    imageRight(
      Sampled.categorical(
        grid,
        NonSpatialAxes.empty,
        NDArray.tabulate[Long](shape(0), shape(1), shape(2)) { (i, j, k) =>
          ((i + 3 * j + 7 * k) % 127).toLong
        }
      )
    )

  private def gridFor(
      shape: Vector[Int]
  ) =
    val frame = geometryRight(Frame.named[D3]("production-codec"))
    geometryRight(Grid.in(frame)(shape, Affine.identity[D3]))

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def imageRight[A](
      value: Either[image4s.ImageError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def niftiRight[A](
      value: Either[NiftiError, A]
  ): A =
    value.fold(error => fail(error.message), identity)
