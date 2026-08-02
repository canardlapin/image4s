package image4s.ops.laws

import image4s.Continuous
import image4s.ImageError
import image4s.MaskImage
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.filter.Gaussian
import image4s.filter.Gradient
import image4s.filter.LinearFilter
import image4s.filter.gaussianBlur
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.morphology.dilate
import image4s.morphology.BinaryMorphology
import image4s.morphology.threshold
import image4s.morphology.StructuringElement
import image4s.ops.Border
import image4s.ops.Correlation
import image4s.ops.FilterExtent
import image4s.ops.IndexCoordinates
import image4s.ops.Kernel
import image4s.ops.OpError
import image4s.ops.Offset
import image4s.ops.Radius
import image4s.ops.SpatialSigma
import image4s.ops.Support
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import ravel.AnyRank
import ravel.DType.given
import ravel.NDArray
import ravel.Rank

/** JVM-side parity oracle and wall-clock baseline for the SciPy harness.
  *
  * The companion Python script owns the external reference implementation.
  * This class deliberately writes plain TSV rather than a Scala-specific
  * serialization so the comparison remains inspectable and language-neutral.
  */
object ImageOpsParityBenchmark:
  private val Small2D = Vector(17, 13)
  private val Small3D = Vector(9, 8, 7)
  private val Benchmark2D = Vector(128, 128)
  private val Benchmark3D = Vector(32, 32, 32)
  private val CrossShape2D = Vector(192, 96)
  private val CrossShape3D = Vector(24, 32, 40)
  private val ConstantExtent = FilterExtent.same(Border.Constant(0.0))
  private val Sigma2D = opsRight(SpatialSigma.samples[D2](1.25))
  private val Sigma3D = opsRight(SpatialSigma.samples[D3](1.0))
  private val Cross2D = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
  private val Ball3D = StructuringElement.ball[D3](opsRight(Radius.samples(1)))
  @volatile private var retained: AnyRef = null

  private final case class Options(
      parityOutput: Path = Paths.get("target/image-ops-parity/scala-parity.tsv"),
      benchmarkOutput: Path = Paths.get("target/image-ops-parity/scala-benchmark.tsv"),
      environmentOutput: Path = Paths.get("target/image-ops-parity/scala-environment.tsv"),
      warmups: Int = 3,
      iterations: Int = 9,
      innerRepetitions: Int = 3
  )

  private final case class ParityCase(
      name: String,
      shape: Vector[Int],
      values: Vector[Double]
  )

  private final case class BenchmarkCase(
      phase: String,
      name: String,
      shape: Vector[Int],
      samples: Long,
      run: () => AnyRef
  )

  private final case class Timing(
      phase: String,
      name: String,
      shape: Vector[Int],
      samples: Long,
      warmups: Int,
      iterations: Int,
      innerRepetitions: Int,
      medianNanos: Long,
      p25Nanos: Long,
      p75Nanos: Long,
      minNanos: Long,
      maxNanos: Long,
      madNanos: Long
  )

  def main(args: Array[String]): Unit =
    val options = parse(args)
    ensureParent(options.parityOutput)
    ensureParent(options.benchmarkOutput)
    ensureParent(options.environmentOutput)
    writeParity(options.parityOutput, parityCases)
    val timings = benchmarkCases.map(runBenchmark(_, options))
    if retained == null then
      throw new IllegalStateException("benchmark produced no retained output")
    writeBenchmarks(options.benchmarkOutput, timings)
    writeEnvironment(options.environmentOutput)
    println(
      s"wrote image4s parity and benchmark artifacts to " +
        s"${options.parityOutput.getParent}"
    )

  private def parityCases: Vector[ParityCase] =
    val image2D = continuous2D("parity-d2", Small2D)
    val image3D = continuous3D("parity-d3", Small3D)
    val sobel2D = opsRight(
      Gradient.sobel(image2D, IndexCoordinates, Border.Constant(0.0))
    )
    val sobel3D = opsRight(
      Gradient.sobel(image3D, IndexCoordinates, Border.Constant(0.0))
    )
    val correlation = opsRight(
      LinearFilter.correlate(
        image2D,
        Correlation[D2, Double](asymmetricKernel, ConstantExtent)
      )
    )
    val dilated2D = opsRight(
      opsRight(image2D.threshold(0.5)).dilate(Cross2D)
    )
    val dilated3D = opsRight(
      opsRight(image3D.threshold(0.5)).dilate(Ball3D)
    )
    Vector(
      ParityCase(
        "gaussian_d2",
        Small2D,
        doubleValues(opsRight(image2D.gaussianBlur(Sigma2D, ConstantExtent)))
      ),
      ParityCase("correlation_d2", Small2D, doubleValues(correlation)),
      ParityCase("sobel_d2_x", Small2D, doubleValues(sobel2D.components(0))),
      ParityCase("sobel_d2_y", Small2D, doubleValues(sobel2D.components(1))),
      ParityCase("dilate_d2", Small2D, maskValues(dilated2D)),
      ParityCase(
        "gaussian_d3",
        Small3D,
        doubleValues(opsRight(image3D.gaussianBlur(Sigma3D, ConstantExtent)))
      ),
      ParityCase("sobel_d3_x", Small3D, doubleValues(sobel3D.components(0))),
      ParityCase("sobel_d3_y", Small3D, doubleValues(sobel3D.components(1))),
      ParityCase("sobel_d3_z", Small3D, doubleValues(sobel3D.components(2))),
      ParityCase("dilate_d3", Small3D, maskValues(dilated3D))
    )

  private def benchmarkCases: Vector[BenchmarkCase] =
    val image2D = continuous2D("benchmark-d2", Benchmark2D)
    val image3D = continuous3D("benchmark-d3", Benchmark3D)
    val correlation = Correlation[D2, Double](asymmetricKernel, ConstantExtent)
    val gaussian2DPlan = opsRight(
      Gaussian.prepare(image2D, Sigma2D, ConstantExtent)
    )
    val gaussian3DPlan = opsRight(
      Gaussian.prepare(image3D, Sigma3D, ConstantExtent)
    )
    val correlationPlan = opsRight(
      LinearFilter.prepareCorrelation(image2D, correlation)
    )
    val threshold2D = opsRight(image2D.threshold(0.5))
    val threshold3D = opsRight(image3D.threshold(0.5))
    val dilation2DPlan = opsRight(
      BinaryMorphology.prepareDilate(threshold2D, Cross2D)
    )
    val dilation3DPlan = opsRight(
      BinaryMorphology.prepareDilate(threshold3D, Ball3D)
    )
    val wide2D = continuous2D("benchmark-d2-wide", CrossShape2D)
    val wide3D = continuous3D("benchmark-d3-wide", CrossShape3D)
    Vector(
      BenchmarkCase(
        "one-shot",
        "gaussian_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () =>
          opsRight(image2D.gaussianBlur(Sigma2D, ConstantExtent))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepare",
        "gaussian_d2",
        Benchmark2D,
        0L,
        () =>
          opsRight(Gaussian.prepare(image2D, Sigma2D, ConstantExtent))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepared-run",
        "gaussian_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () => opsRight(gaussian2DPlan.run(image2D)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "correlation_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () =>
          opsRight(LinearFilter.correlate(image2D, correlation))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepare",
        "correlation_d2",
        Benchmark2D,
        0L,
        () =>
          opsRight(LinearFilter.prepareCorrelation(image2D, correlation))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepared-run",
        "correlation_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () => opsRight(correlationPlan.run(image2D)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "sobel_d2_full",
        Vector(2) ++ Benchmark2D,
        Benchmark2D.product.toLong * 2L,
        () =>
          opsRight(
            Gradient.sobel(
              image2D,
              IndexCoordinates,
              Border.Constant(0.0)
            )
          ).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "threshold_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () => opsRight(image2D.threshold(0.5)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "dilate_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () =>
          opsRight(opsRight(image2D.threshold(0.5)).dilate(Cross2D))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepare",
        "dilate_d2",
        Benchmark2D,
        0L,
        () =>
          opsRight(BinaryMorphology.prepareDilate(threshold2D, Cross2D))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepared-run",
        "dilate_d2",
        Benchmark2D,
        Benchmark2D.product.toLong,
        () => opsRight(dilation2DPlan.run(threshold2D)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "gaussian_d3",
        Benchmark3D,
        Benchmark3D.product.toLong,
        () =>
          opsRight(image3D.gaussianBlur(Sigma3D, ConstantExtent))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepare",
        "gaussian_d3",
        Benchmark3D,
        0L,
        () =>
          opsRight(Gaussian.prepare(image3D, Sigma3D, ConstantExtent))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepared-run",
        "gaussian_d3",
        Benchmark3D,
        Benchmark3D.product.toLong,
        () => opsRight(gaussian3DPlan.run(image3D)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "sobel_d3_full",
        Vector(3) ++ Benchmark3D,
        Benchmark3D.product.toLong * 3L,
        () =>
          opsRight(
            Gradient.sobel(
              image3D,
              IndexCoordinates,
              Border.Constant(0.0)
            )
          ).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "threshold_d3",
        Benchmark3D,
        Benchmark3D.product.toLong,
        () => opsRight(image3D.threshold(0.5)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "dilate_d3",
        Benchmark3D,
        Benchmark3D.product.toLong,
        () =>
          opsRight(opsRight(image3D.threshold(0.5)).dilate(Ball3D))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepare",
        "dilate_d3",
        Benchmark3D,
        0L,
        () =>
          opsRight(BinaryMorphology.prepareDilate(threshold3D, Ball3D))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "prepared-run",
        "dilate_d3",
        Benchmark3D,
        Benchmark3D.product.toLong,
        () => opsRight(dilation3DPlan.run(threshold3D)).asInstanceOf[AnyRef]
      )
    ) ++ Vector(
      BenchmarkCase(
        "one-shot",
        "gaussian_d2_wide",
        CrossShape2D,
        CrossShape2D.product.toLong,
        () =>
          opsRight(wide2D.gaussianBlur(Sigma2D, ConstantExtent))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "correlation_d2_wide",
        CrossShape2D,
        CrossShape2D.product.toLong,
        () =>
          opsRight(LinearFilter.correlate(wide2D, correlation))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "sobel_d2_wide_full",
        Vector(2) ++ CrossShape2D,
        CrossShape2D.product.toLong * 2L,
        () =>
          opsRight(
            Gradient.sobel(
              wide2D,
              IndexCoordinates,
              Border.Constant(0.0)
            )
          ).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "threshold_d2_wide",
        CrossShape2D,
        CrossShape2D.product.toLong,
        () => opsRight(wide2D.threshold(0.5)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "dilate_d2_wide",
        CrossShape2D,
        CrossShape2D.product.toLong,
        () =>
          opsRight(opsRight(wide2D.threshold(0.5)).dilate(Cross2D))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "gaussian_d3_wide",
        CrossShape3D,
        CrossShape3D.product.toLong,
        () =>
          opsRight(wide3D.gaussianBlur(Sigma3D, ConstantExtent))
            .asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "sobel_d3_wide_full",
        Vector(3) ++ CrossShape3D,
        CrossShape3D.product.toLong * 3L,
        () =>
          opsRight(
            Gradient.sobel(
              wide3D,
              IndexCoordinates,
              Border.Constant(0.0)
            )
          ).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "threshold_d3_wide",
        CrossShape3D,
        CrossShape3D.product.toLong,
        () => opsRight(wide3D.threshold(0.5)).asInstanceOf[AnyRef]
      ),
      BenchmarkCase(
        "one-shot",
        "dilate_d3_wide",
        CrossShape3D,
        CrossShape3D.product.toLong,
        () =>
          opsRight(opsRight(wide3D.threshold(0.5)).dilate(Ball3D))
            .asInstanceOf[AnyRef]
      )
    )

  private def asymmetricKernel: Kernel[D2, Double] =
    val offsets =
      for
        x <- -1 to 1
        y <- -1 to 1
      yield Offset.unsafe[D2](Vector(x, y))
    val support = opsRight(Support.create[D2](offsets))
    val weights = support.offsets.map { offset =>
      val x = offset.coordinates(0).toDouble
      val y = offset.coordinates(1).toDouble
      1.0 + 0.2 * x + 0.1 * y + 0.05 * x * y
    }
    opsRight(Kernel.sparse(support, weights))

  private def continuous2D(label: String, shape: Vector[Int]) =
    val frame = geometryRight(Frame.named[D2](label))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[2]](
        space,
        NDArray.tabulate[Double](shape(0), shape(1)) { (row, column) =>
          ((row * 3 + column * 5) % 29).toDouble / 29.0
        }
      )
    )

  private def continuous3D(label: String, shape: Vector[Int]) =
    val frame = geometryRight(Frame.named[D3](label))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D3]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[3]](
        space,
        NDArray.tabulate[Double](shape(0), shape(1), shape(2)) {
          (x, y, z) =>
            ((x + 2 * y + 3 * z) % 31).toDouble / 31.0
        }
      )
    )

  private def doubleValues(
      image: Sampled[?, Double, Continuous, ?]
  ): Vector[Double] =
    image.data.elementsIterator.toVector

  private def maskValues[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](image: MaskImage[S, R]): Vector[Double] =
    image.data.elementsIterator.map(value => if value then 1.0 else 0.0).toVector

  private def runBenchmark(
      benchmark: BenchmarkCase,
      options: Options
  ): Timing =
    var warmup = 0
    while warmup < options.warmups do
      runRepeated(benchmark.run, options.innerRepetitions)
      warmup += 1
    val timings = Array.ofDim[Long](options.iterations)
    var iteration = 0
    while iteration < options.iterations do
      val started = System.nanoTime()
      runRepeated(benchmark.run, options.innerRepetitions)
      timings(iteration) =
        math.max(
          1L,
          (System.nanoTime() - started) / options.innerRepetitions.toLong
        )
      iteration += 1
    val sorted = timings.sorted
    val median = sorted(sorted.length / 2)
    val deviations = timings.map(value => math.abs(value - median)).sorted
    Timing(
      benchmark.phase,
      benchmark.name,
      benchmark.shape,
      benchmark.samples,
      options.warmups,
      options.iterations,
      options.innerRepetitions,
      median,
      sorted((sorted.length - 1) * 25 / 100),
      sorted((sorted.length - 1) * 75 / 100),
      sorted.head,
      sorted.last,
      deviations(deviations.length / 2)
    )

  private def runRepeated(operation: () => AnyRef, repetitions: Int): Unit =
    var repetition = 0
    while repetition < repetitions do
      retained = operation()
      repetition += 1

  private def writeParity(path: Path, cases: Vector[ParityCase]): Unit =
    val lines =
      Vector("operation\tshape\tvalues") ++ cases.map { current =>
        val shape = current.shape.mkString(",")
        val values = current.values.map(java.lang.Double.toString).mkString(",")
        s"${current.name}\t$shape\t$values"
      }
    write(path, lines.mkString("\n") + "\n")

  private def writeBenchmarks(path: Path, timings: Vector[Timing]): Unit =
    val lines =
      Vector(
        "implementation\tphase\toperation\tshape\tsamples\twarmups\titerations\t" +
          "inner_repetitions\tmedian_ns\tp25_ns\tp75_ns\tmin_ns\tmax_ns\tmad_ns"
      ) ++ timings.map { timing =>
        s"scala-jvm\t${timing.phase}\t${timing.name}\t${timing.shape.mkString(",")}\t" +
          s"${timing.samples}\t${timing.warmups}\t${timing.iterations}\t" +
          s"${timing.innerRepetitions}\t${timing.medianNanos}\t" +
          s"${timing.p25Nanos}\t${timing.p75Nanos}\t${timing.minNanos}\t" +
          s"${timing.maxNanos}\t${timing.madNanos}"
      }
    write(path, lines.mkString("\n") + "\n")

  private def writeEnvironment(path: Path): Unit =
    write(
      path,
      Vector(
        s"java\t${System.getProperty("java.version")}",
        s"scala\t${scala.util.Properties.versionNumberString}",
        s"os\t${System.getProperty("os.name")}",
        s"arch\t${System.getProperty("os.arch")}"
      ).mkString("\n") + "\n"
    )

  private def parse(args: Array[String]): Options =
    var options = Options()
    var index = 0
    while index < args.length do
      args(index) match
        case "--parity-output" =>
          options = options.copy(parityOutput = Paths.get(next(args, index)))
          index += 1
        case "--benchmark-output" =>
          options = options.copy(benchmarkOutput = Paths.get(next(args, index)))
          index += 1
        case "--environment-output" =>
          options = options.copy(environmentOutput = Paths.get(next(args, index)))
          index += 1
        case "--warmups" =>
          options = options.copy(warmups = next(args, index).toInt)
          index += 1
        case "--iterations" =>
          options = options.copy(iterations = next(args, index).toInt)
          index += 1
        case "--inner-repetitions" =>
          options = options.copy(innerRepetitions = next(args, index).toInt)
          index += 1
        case unknown =>
          throw new IllegalArgumentException(s"unknown argument: $unknown")
      index += 1
    if
      options.warmups < 0 ||
        options.iterations <= 0 ||
        options.innerRepetitions <= 0
    then
      throw new IllegalArgumentException(
        "warmups must be non-negative, iterations and inner-repetitions must be positive"
      )
    options

  private def next(args: Array[String], index: Int): String =
    if index + 1 >= args.length then
      throw new IllegalArgumentException(s"missing value for ${args(index)}")
    args(index + 1)

  private def ensureParent(path: Path): Unit =
    val parent = path.getParent
    if parent != null then
      Files.createDirectories(parent)
      ()

  private def write(path: Path, content: String): Unit =
    Files.writeString(path, content, StandardCharsets.UTF_8)
    ()

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => throw new IllegalStateException(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => throw new IllegalStateException(error.message)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => throw new IllegalStateException(error.message)
