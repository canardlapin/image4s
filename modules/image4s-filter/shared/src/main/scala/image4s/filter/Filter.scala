package image4s.filter

import image4s.Continuous
import image4s.ContinuousImage
import image4s.SampleSpace
import image4s.Sampled
import image4s.ValueSemantics
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import image4s.ops.AxisKernel
import image4s.ops.Border
import image4s.ops.Convolution
import image4s.ops.Correlation
import image4s.ops.ExecutionPolicy
import image4s.ops.FilterExtent
import image4s.ops.FilterMethod
import image4s.ops.Kernel
import image4s.ops.OpError
import image4s.ops.OutputGrid
import image4s.ops.PlanReport
import image4s.ops.SelectedMethod
import image4s.ops.SpatialSigma
import image4s.ops.Support
import ravel.AnyRank
import ravel.FloatingDType
import ravel.MutableNDArray
import ravel.NDArray
import ravel.NumericDType
import ravel.Shape
import ravel.stencil.BorderMode
import ravel.stencil.DirectNeighborhoodExecutor
import ravel.stencil.DoubleNeighborhoodReducer
import ravel.stencil.FloatNeighborhoodReducer
import ravel.stencil.NeighborhoodSpec

private[filter] trait PreparedPrimitiveFilter[A, R <: AnyRank]:
  def run(
      source: NDArray[A, R],
      destination: MutableNDArray[A, R]
  ): Unit

  def runMutable(
      source: MutableNDArray[A, R],
      destination: MutableNDArray[A, R]
  ): Unit

private[filter] trait PreparedFilterExecution[
    A,
    R <: AnyRank
]:
  def run(source: NDArray[A, R]): MutableNDArray[A, R]

/** Closed primitive output family for linear filters.
  *
  * Dispatch happens once while preparing the pass. Float and Double reducers
  * then retain primitive weights and accumulators throughout the stencil loop.
  */
sealed trait FilterOutput[A]:
  private[filter] def zero: A
  private[filter] def one: A
  private[filter] def fromDouble(value: Double): A
  private[filter] def add(left: A, right: A): A
  private[filter] def multiply(left: A, right: A): A
  private[filter] def bytesPerElement: Int

  private[filter] def run[R <: AnyRank](
      source: NDArray[A, R],
      destination: MutableNDArray[A, R],
      spec: NeighborhoodSpec,
      weights: Vector[A],
      constant: A
  ): Unit =
    prepare(source, destination, spec, weights, constant)
      .run(source, destination)

  private[filter] def prepare[R <: AnyRank](
      source: NDArray[A, R],
      destination: MutableNDArray[A, R],
      spec: NeighborhoodSpec,
      weights: Vector[A],
      constant: A
  ): PreparedPrimitiveFilter[A, R]

  private[filter] def prepareMutable[R <: AnyRank](
      source: MutableNDArray[A, R],
      destination: MutableNDArray[A, R],
      spec: NeighborhoodSpec,
      weights: Vector[A],
      constant: A
  ): PreparedPrimitiveFilter[A, R]

object FilterOutput:
  given float: FilterOutput[Float] with
    private[filter] val zero: Float = 0.0f
    private[filter] val one: Float = 1.0f
    private[filter] val bytesPerElement: Int = 4

    private[filter] def fromDouble(value: Double): Float =
      value.toFloat

    private[filter] def add(left: Float, right: Float): Float =
      left + right

    private[filter] def multiply(left: Float, right: Float): Float =
      left * right

    private[filter] def prepare[R <: AnyRank](
        source: NDArray[Float, R],
        destination: MutableNDArray[Float, R],
        spec: NeighborhoodSpec,
        weights: Vector[Float],
        constant: Float
    ): PreparedPrimitiveFilter[Float, R] =
      val primitiveWeights = weights.toArray
      val reducer = new FloatNeighborhoodReducer:
        def zero: Float =
          0.0f

        def accumulate(
            accumulator: Float,
            value: Float,
            offsetIndex: Int
        ): Float =
          accumulator + value * primitiveWeights(offsetIndex)

        def finish(accumulator: Float): Float =
          accumulator

      val prepared =
        DirectNeighborhoodExecutor.prepare(source, destination, spec)
      new PreparedPrimitiveFilter[Float, R]:
        def run(
            nextSource: NDArray[Float, R],
            nextDestination: MutableNDArray[Float, R]
        ): Unit =
          prepared.runFloat(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

        def runMutable(
            nextSource: MutableNDArray[Float, R],
            nextDestination: MutableNDArray[Float, R]
        ): Unit =
          prepared.runFloat(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

    private[filter] def prepareMutable[R <: AnyRank](
        source: MutableNDArray[Float, R],
        destination: MutableNDArray[Float, R],
        spec: NeighborhoodSpec,
        weights: Vector[Float],
        constant: Float
    ): PreparedPrimitiveFilter[Float, R] =
      val primitiveWeights = weights.toArray
      val reducer = new FloatNeighborhoodReducer:
        def zero: Float =
          0.0f

        def accumulate(
            accumulator: Float,
            value: Float,
            offsetIndex: Int
        ): Float =
          accumulator + value * primitiveWeights(offsetIndex)

        def finish(accumulator: Float): Float =
          accumulator

      val prepared =
        DirectNeighborhoodExecutor.prepare(source, destination, spec)
      new PreparedPrimitiveFilter[Float, R]:
        def run(
            nextSource: NDArray[Float, R],
            nextDestination: MutableNDArray[Float, R]
        ): Unit =
          prepared.runFloat(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

        def runMutable(
            nextSource: MutableNDArray[Float, R],
            nextDestination: MutableNDArray[Float, R]
        ): Unit =
          prepared.runFloat(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

  given double: FilterOutput[Double] with
    private[filter] val zero: Double = 0.0
    private[filter] val one: Double = 1.0
    private[filter] val bytesPerElement: Int = 8

    private[filter] def fromDouble(value: Double): Double =
      value

    private[filter] def add(left: Double, right: Double): Double =
      left + right

    private[filter] def multiply(left: Double, right: Double): Double =
      left * right

    private[filter] def prepare[R <: AnyRank](
        source: NDArray[Double, R],
        destination: MutableNDArray[Double, R],
        spec: NeighborhoodSpec,
        weights: Vector[Double],
        constant: Double
    ): PreparedPrimitiveFilter[Double, R] =
      val primitiveWeights = weights.toArray
      val reducer = new DoubleNeighborhoodReducer:
        def zero: Double =
          0.0

        def accumulate(
            accumulator: Double,
            value: Double,
            offsetIndex: Int
        ): Double =
          accumulator + value * primitiveWeights(offsetIndex)

        def finish(accumulator: Double): Double =
          accumulator

      val prepared =
        DirectNeighborhoodExecutor.prepare(source, destination, spec)
      new PreparedPrimitiveFilter[Double, R]:
        def run(
            nextSource: NDArray[Double, R],
            nextDestination: MutableNDArray[Double, R]
        ): Unit =
          prepared.runDouble(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

        def runMutable(
            nextSource: MutableNDArray[Double, R],
            nextDestination: MutableNDArray[Double, R]
        ): Unit =
          prepared.runDouble(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

    private[filter] def prepareMutable[R <: AnyRank](
        source: MutableNDArray[Double, R],
        destination: MutableNDArray[Double, R],
        spec: NeighborhoodSpec,
        weights: Vector[Double],
        constant: Double
    ): PreparedPrimitiveFilter[Double, R] =
      val primitiveWeights = weights.toArray
      val reducer = new DoubleNeighborhoodReducer:
        def zero: Double =
          0.0

        def accumulate(
            accumulator: Double,
            value: Double,
            offsetIndex: Int
        ): Double =
          accumulator + value * primitiveWeights(offsetIndex)

        def finish(accumulator: Double): Double =
          accumulator

      val prepared =
        DirectNeighborhoodExecutor.prepare(source, destination, spec)
      new PreparedPrimitiveFilter[Double, R]:
        def run(
            nextSource: NDArray[Double, R],
            nextDestination: MutableNDArray[Double, R]
        ): Unit =
          prepared.runDouble(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

        def runMutable(
            nextSource: MutableNDArray[Double, R],
            nextDestination: MutableNDArray[Double, R]
        ): Unit =
          prepared.runDouble(
            nextSource,
            nextDestination,
            reducer,
            constant
          )

/** Sequential reusable filter schedule.
  *
  * The plan owns one mutable primitive workspace and reuses Ravel's prepared
  * address schedule. Each returned image receives a fresh immutable buffer, so
  * later runs cannot change earlier results. A plan is not thread-safe; create
  * one plan per concurrent caller.
  */
final class PreparedLinearFilter[
    D <: Dim,
    F <: Frame[D],
    S <: SampleSpace[?, ?],
    A,
    B,
    R <: AnyRank
] private[filter] (
    expectedSpace: S,
    outputGrid: Grid[F, D],
    execution: PreparedFilterExecution[B, R],
    convert: NDArray[A, R] => NDArray[B, R],
    val report: PlanReport
)(using semantics: ValueSemantics[B, Continuous]):
  def run[S2 <: SampleSpace[?, ?]](
      input: Sampled[S2, A, Continuous, R]
  ): Either[
    OpError,
    ContinuousImage[? <: SampleSpace[F, D], B, R]
  ] =
    if !(input.sampleSpace eq expectedSpace) then
      Left(
        OpError.InvalidArgument(
          "prepared filter requires the same live sample-space owner"
        )
      )
    else
      val source = convert(input.data)
      val workspace = execution.run(source)
      Sampled
        .continuous(
          outputGrid,
          input.nonSpatialAxes,
          workspace.freezeCopy(),
          input.metadata
        )
        .left
        .map(OpError.Image.apply)

object LinearFilter:
  def prepareCorrelation[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operation: Correlation[input.sampleSpace.D, A],
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      A,
      R
    ]
  ] =
    prepare(
      input,
      input.data,
      operation.kernel,
      operation.extent,
      reverse = false,
      inputMaterialized = false,
      identity,
      policy
    )

  def prepareCorrelationTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operation: Correlation[input.sampleSpace.D, B],
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      B,
      R
    ]
  ] =
    val converted = input.data.cast[B]
    prepare(
      input,
      converted,
      operation.kernel,
      operation.extent,
      reverse = false,
      inputMaterialized = true,
      _.cast[B],
      policy
    )

  def correlate[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operation: Correlation[input.sampleSpace.D, A],
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      A,
      R
    ]
  ] =
    prepareCorrelation(input, operation, policy).flatMap(_.run(input))

  def convolve[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operation: Convolution[input.sampleSpace.D, A],
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      A,
      R
    ]
  ] =
    execute(
      input,
      input.data,
      operation.kernel,
      operation.extent,
      reverse = true,
      policy
    )

  def correlateTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operation: Correlation[input.sampleSpace.D, B],
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      B,
      R
    ]
  ] =
    prepareCorrelationTo(input, operation, policy).flatMap(_.run(input))

  def convolveTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      operation: Convolution[input.sampleSpace.D, B],
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      B,
      R
    ]
  ] =
    execute(
      input,
      input.data.cast[B],
      operation.kernel,
      operation.extent,
      reverse = true,
      policy
    )

  private def execute[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      source: NDArray[B, R],
      kernel: Kernel[input.sampleSpace.D, B],
      extent: FilterExtent[B],
      reverse: Boolean,
      policy: ExecutionPolicy
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      B,
      R
    ]
  ] =
    prepare(
      input,
      source,
      kernel,
      extent,
      reverse,
      inputMaterialized = source.asInstanceOf[AnyRef] ne input.data.asInstanceOf[AnyRef],
      _ => source,
      policy
    ).flatMap(_.run(input))

  private def prepare[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      source: NDArray[B, R],
      kernel: Kernel[input.sampleSpace.D, B],
      extent: FilterExtent[B],
      reverse: Boolean,
      inputMaterialized: Boolean,
      convert: NDArray[A, R] => NDArray[B, R],
      policy: ExecutionPolicy
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      B,
      R
    ]
  ] =
    policy.method match
      case FilterMethod.Fft =>
        Left(OpError.InvalidArgument("FFT filtering is not implemented"))
      case FilterMethod.Direct =>
        prepareDirect(
          input,
          source,
          kernel,
          extent,
          reverse,
          inputMaterialized,
          convert
        )
      case FilterMethod.Auto =>
        kernel match
          case separable: Kernel.Separable[input.sampleSpace.D, B]
              if extent.isInstanceOf[FilterExtent.Same[?]] =>
            prepareSeparable(
              input,
              source,
              separable,
              extent,
              reverse,
              inputMaterialized,
              convert
            )
          case _ =>
            prepareDirect(
              input,
              source,
              kernel,
              extent,
              reverse,
              inputMaterialized,
              convert
            )
      case FilterMethod.Separable =>
        kernel match
          case separable: Kernel.Separable[input.sampleSpace.D, B] =>
            prepareSeparable(
              input,
              source,
              separable,
              extent,
              reverse,
              inputMaterialized,
              convert
            )
          case _ =>
            Left(
              OpError.InvalidArgument(
                "ExecutionPolicy.Separable requires Kernel.Separable"
              )
            )

  private def prepareDirect[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      source: NDArray[B, R],
      kernel: Kernel[input.sampleSpace.D, B],
      extent: FilterExtent[B],
      reverse: Boolean,
      inputMaterialized: Boolean,
      convert: NDArray[A, R] => NDArray[B, R]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      B,
      R
    ]
  ] =
    for
      weighted <- weightedOffsets(kernel, reverse)
      (offsets, weights, effectiveSupport) = weighted
      outputGrid <- OutputGrid.grid(input.grid, effectiveSupport, extent)
      shape <- outputShape(input, outputGrid)
      border = borderConfiguration(extent)
      spec = NeighborhoodSpec(
        spatialAxes = dimension.rank,
        offsets = offsets,
        border = border._1,
        outputOrigin = outputOrigin(effectiveSupport, extent),
        outputSpatialShape = outputGrid.shape
      )
      workspace = MutableNDArray.zeros[B, R](shape)
      pass =
        target.prepare(source, workspace, spec, weights, border._2)
      execution = new PreparedFilterExecution[B, R]:
        def run(nextSource: NDArray[B, R]): MutableNDArray[B, R] =
          pass.run(nextSource, workspace)
          workspace
      plan = new PreparedLinearFilter[
        input.sampleSpace.D,
        input.sampleSpace.F,
        S,
        A,
        B,
        R
      ](
        input.sampleSpace,
        outputGrid,
        execution,
        convert,
        PlanReport(
          method = SelectedMethod.Direct,
          passes = 1,
          inputMaterialized = inputMaterialized,
          outputShape = Vector.tabulate(shape.rank)(shape.apply),
          workspaceBytes =
            shape.size.toLong * target.bytesPerElement.toLong
        )
      )
    yield plan

  private def prepareSeparable[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      source: NDArray[B, R],
      kernel: Kernel.Separable[input.sampleSpace.D, B],
      extent: FilterExtent[B],
      reverse: Boolean,
      inputMaterialized: Boolean,
      convert: NDArray[A, R] => NDArray[B, R]
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      B,
      R
    ]
  ] =
    extent match
      case FilterExtent.Same(sameBorder) =>
        for
          outputGrid <- OutputGrid.grid(input.grid, kernel.support, extent)
          shape <- outputShape(input, outputGrid)
          firstWorkspace = MutableNDArray.zeros[B, R](shape)
          secondWorkspace = MutableNDArray.zeros[B, R](shape)
          border = borderValue(sameBorder)
          execution <- separableExecution(
            source,
            firstWorkspace,
            secondWorkspace,
            kernel.axes,
            reverse,
            border
          )
        yield new PreparedLinearFilter[
          input.sampleSpace.D,
          input.sampleSpace.F,
          S,
          A,
          B,
          R
        ](
          input.sampleSpace,
          outputGrid,
          execution,
          convert,
          PlanReport(
            method = SelectedMethod.Separable,
            passes = dimension.rank,
            inputMaterialized = inputMaterialized,
            outputShape = Vector.tabulate(shape.rank)(shape.apply),
            workspaceBytes =
              shape.size.toLong * target.bytesPerElement.toLong * 2L
          )
        )
      case _ =>
        Left(
          OpError.InvalidArgument(
            "optimized separable execution currently requires FilterExtent.Same"
          )
        )

  private def separableExecution[
      D <: Dim,
      B,
      R <: AnyRank
  ](
      source: NDArray[B, R],
      firstWorkspace: MutableNDArray[B, R],
      secondWorkspace: MutableNDArray[B, R],
      axes: Vector[AxisKernel[B]],
      reverse: Boolean,
      border: (BorderMode, B)
  )(using
      dimension: Dimension[D],
      target: FilterOutput[B]
  ): Either[OpError, PreparedFilterExecution[B, R]] =
    val rank = source.rank
    if axes.length != dimension.rank then
      Left(
        OpError.InvalidKernel(
          s"separable kernel has ${axes.length} factors for spatial rank ${dimension.rank}"
        )
      )
    else
      val orders =
        Vector.tabulate(dimension.rank)(axis => axisOrder(rank, axis))
      val specs =
        axes.zipWithIndex.map { case (axis, factorAxis) =>
          NeighborhoodSpec(
            spatialAxes = 1,
            offsets =
              axis.weights.indices.map { index =>
                val offset = index - axis.anchor
                Vector(if reverse then -offset else offset)
              }.toVector,
            border = border._1,
            outputOrigin = Vector(0),
            outputSpatialShape = Vector(source.shape(factorAxis))
          )
        }
      val constants =
        axes.scanLeft(border._2) { (constant, axis) =>
          target.multiply(constant, sumWeights(axis.weights))
        }.dropRight(1)
      val firstPass =
        target.prepare(
          source.permuteAxes(orders(0)*),
          firstWorkspace.permuteAxes(orders(0)*),
          specs(0),
          axes(0).weights,
          constants(0)
        )
      val remaining =
        Vector.tabulate(dimension.rank - 1) { index =>
          val axis = index + 1
          val sourceWorkspace =
            if axis % 2 == 1 then firstWorkspace else secondWorkspace
          val destinationWorkspace =
            if axis % 2 == 1 then secondWorkspace else firstWorkspace
          target.prepareMutable(
            sourceWorkspace.permuteAxes(orders(axis)*),
            destinationWorkspace.permuteAxes(orders(axis)*),
            specs(axis),
            axes(axis).weights,
            constants(axis)
          )
        }
      Right(new PreparedFilterExecution[B, R]:
        def run(nextSource: NDArray[B, R]): MutableNDArray[B, R] =
          firstPass.run(
            nextSource.permuteAxes(orders(0)*),
            firstWorkspace.permuteAxes(orders(0)*)
          )
          var current = firstWorkspace
          var axis = 1
          while axis < dimension.rank do
            val destination =
              if current eq firstWorkspace then secondWorkspace
              else firstWorkspace
            remaining(axis - 1).runMutable(
              current.permuteAxes(orders(axis)*),
              destination.permuteAxes(orders(axis)*)
            )
            current = destination
            axis += 1
          current
      )

  private def axisOrder(rank: Int, leadingAxis: Int): Vector[Int] =
    leadingAxis +: (0 until rank).filter(_ != leadingAxis).toVector

  private def sumWeights[A](
      weights: Vector[A]
  )(using target: FilterOutput[A]): A =
    var sum = target.zero
    var index = 0
    while index < weights.length do
      sum = target.add(sum, weights(index))
      index += 1
    sum

  private def weightedOffsets[D <: Dim, A](
      kernel: Kernel[D, A],
      reverse: Boolean
  )(using
      dimension: Dimension[D],
      target: FilterOutput[A]
  ): Either[
    OpError,
    (Vector[Vector[Int]], Vector[A], Support[D])
  ] =
    val support = kernel.support
    val weights =
      kernel match
        case dense: Kernel.Dense[D, A] =>
          dense.weights
        case sparse: Kernel.Sparse[D, A] =>
          sparse.weights
        case separable: Kernel.Separable[D, A] =>
          support.offsets.map { offset =>
            var product = target.one
            var axis = 0
            while axis < dimension.rank do
              val factor = separable.axes(axis)
              val index = offset.coordinates(axis) + factor.anchor
              product = target.multiply(product, factor.weights(index))
              axis += 1
            product
          }
    val offsets =
      support.offsets.map { offset =>
        if reverse then offset.coordinates.map(value => -value)
        else offset.coordinates
      }
    if reverse then
      Support
        .create(
          offsets.map(coordinates =>
            image4s.ops.Offset.unsafe[D](coordinates)
          )
        )
        .map(reversed => (offsets, weights, reversed))
    else Right((offsets, weights, support))

  private def outputShape[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      outputGrid: Grid[input.sampleSpace.F, input.sampleSpace.D]
  )(using
      dimension: Dimension[input.sampleSpace.D]
  ): Either[OpError, Shape[R]] =
    val dimensions =
      outputGrid.shape ++ input.logicalShape.drop(dimension.rank)
    Shape
      .from(dimensions)
      .left
      .map(error => OpError.InvalidArgument(error.toString))
      .map(_.asInstanceOf[Shape[R]])

  private def outputOrigin[D <: Dim, A](
      support: Support[D],
      extent: FilterExtent[A]
  )(using dimension: Dimension[D]): Vector[Int] =
    extent match
      case FilterExtent.Same(_) =>
        Vector.fill(dimension.rank)(0)
      case FilterExtent.Valid =>
        support.leftExtents
      case FilterExtent.Full(_) =>
        support.leftExtents.map(value => -value)

  private def borderConfiguration[A](
      extent: FilterExtent[A]
  )(using target: FilterOutput[A]): (BorderMode, A) =
    extent match
      case FilterExtent.Valid =>
        (BorderMode.Constant, target.zero)
      case FilterExtent.Same(border) =>
        borderValue(border)
      case FilterExtent.Full(border) =>
        borderValue(border)

  private def borderValue[A](
      border: Border[A]
  )(using target: FilterOutput[A]): (BorderMode, A) =
    border match
      case Border.Constant(value) =>
        (BorderMode.Constant, value)
      case Border.Replicate =>
        (BorderMode.Replicate, target.zero)
      case Border.ReflectWithoutEdge =>
        (BorderMode.ReflectWithoutEdge, target.zero)
      case Border.ReflectWithEdge =>
        (BorderMode.ReflectWithEdge, target.zero)
      case Border.Wrap =>
        (BorderMode.Wrap, target.zero)

object Gaussian:
  def prepare[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      sigma: SpatialSigma[input.sampleSpace.D],
      extent: FilterExtent[A] = FilterExtent.same(Border.reflect),
      truncate: Double = 3.0,
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      A,
      R
    ]
  ] =
    for
      sampleSigmas <- sampleSigmas(input.grid, sigma)
      kernel <-
        gaussianKernel[input.sampleSpace.D, A](sampleSigmas, truncate)
      plan <-
        LinearFilter.prepareCorrelation(
          input,
          Correlation(kernel, extent),
          policy
        )
    yield plan

  def prepareTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      sigma: SpatialSigma[input.sampleSpace.D],
      extent: FilterExtent[B] = FilterExtent.same(Border.reflect),
      truncate: Double = 3.0,
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    PreparedLinearFilter[
      input.sampleSpace.D,
      input.sampleSpace.F,
      S,
      A,
      B,
      R
    ]
  ] =
    for
      sampleSigmas <- sampleSigmas(input.grid, sigma)
      kernel <-
        gaussianKernel[input.sampleSpace.D, B](sampleSigmas, truncate)
      plan <-
        LinearFilter.prepareCorrelationTo(
          input,
          Correlation(kernel, extent),
          policy
        )
    yield plan

  def blur[
      S <: SampleSpace[?, ?],
      A,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      sigma: SpatialSigma[input.sampleSpace.D],
      extent: FilterExtent[A] = FilterExtent.same(Border.reflect),
      truncate: Double = 3.0,
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      target: FilterOutput[A],
      floating: FloatingDType[A],
      semantics: ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      A,
      R
    ]
  ] =
    prepare(input, sigma, extent, truncate, policy).flatMap(_.run(input))

  def blurTo[
      S <: SampleSpace[?, ?],
      A,
      B,
      R <: AnyRank
  ](
      input: Sampled[S, A, Continuous, R],
      sigma: SpatialSigma[input.sampleSpace.D],
      extent: FilterExtent[B] = FilterExtent.same(Border.reflect),
      truncate: Double = 3.0,
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      dimension: Dimension[input.sampleSpace.D],
      source: NumericDType[A],
      target: FilterOutput[B],
      floating: FloatingDType[B],
      semantics: ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      B,
      R
    ]
  ] =
    prepareTo(input, sigma, extent, truncate, policy).flatMap(_.run(input))

  private def gaussianKernel[D <: Dim, A](
      sigmas: Vector[Double],
      truncate: Double
  )(using
      dimension: Dimension[D],
      target: FilterOutput[A]
  ): Either[OpError, Kernel.Separable[D, A]] =
    if !truncate.isFinite || truncate <= 0.0 then
      Left(
        OpError.InvalidScale(
          s"Gaussian truncation must be positive finite, got $truncate"
        )
      )
    else
      val axes = sigmas.map { sigma =>
        val radius = math.ceil(sigma * truncate).toInt.max(1)
        val raw =
          Vector.tabulate(radius * 2 + 1) { index =>
            val coordinate = index - radius
            math.exp(
              -(coordinate.toDouble * coordinate.toDouble) /
                (2.0 * sigma * sigma)
            )
          }
        val total = raw.sum
        AxisKernel.centered(raw.map(value => target.fromDouble(value / total)))
      }
      axes.collectFirst { case Left(error) => error } match
        case Some(error) =>
          Left(error)
        case None =>
          Kernel.separable(axes.collect { case Right(axis) => axis })

  private def sampleSigmas[D <: Dim, F <: Frame[D]](
      grid: Grid[F, D],
      sigma: SpatialSigma[D]
  )(using dimension: Dimension[D]): Either[OpError, Vector[Double]] =
    sigma match
      case samples: SpatialSigma.Samples[D] =>
        Right(samples.values)
      case frame: SpatialSigma.FrameUnits[D] =>
        val converted =
          frame.values.map(value =>
            value * unitScale(frame.unit.getOrElse(grid.frame.unit)) /
              unitScale(grid.frame.unit)
          )
        frameToSamples(grid, converted)

  private def frameToSamples[D <: Dim, F <: Frame[D]](
      grid: Grid[F, D],
      frameSigmas: Vector[Double]
  )(using dimension: Dimension[D]): Either[OpError, Vector[Double]] =
    val rank = dimension.rank
    val matrix = grid.indexToFrame.rowMajor
    val columns =
      Vector.tabulate(rank) { column =>
        Vector.tabulate(rank) { row =>
          matrix(row * (rank + 1) + column)
        }
      }
    val norms =
      columns.map(column => math.sqrt(column.map(value => value * value).sum))
    if norms.exists(value => !value.isFinite || value <= 0.0) then
      Left(OpError.InvalidScale("grid has a degenerate spatial axis"))
    else
      val tolerance = 1.0e-10
      val isotropic =
        frameSigmas.forall(value =>
          math.abs(value - frameSigmas.head) <=
            tolerance * frameSigmas.head.abs.max(1.0)
        )
      val orthogonal =
        columns.indices.forall { left =>
          columns.indices.forall { right =>
            if left == right then true
            else
              val dot =
                columns(left)
                  .zip(columns(right))
                  .map(_ * _)
                  .sum
              math.abs(dot) <=
                tolerance * norms(left) * norms(right)
          }
        }
      if isotropic && orthogonal then
        Right(
          Vector.tabulate(rank)(axis => frameSigmas.head / norms(axis))
        )
      else
        val rowForColumn =
          columns.map { column =>
            column.indices.filter(row => math.abs(column(row)) > tolerance)
          }
        val rows = rowForColumn.flatten
        if rowForColumn.forall(_.length == 1) && rows.distinct.length == rank then
          Right(
            Vector.tabulate(rank) { axis =>
              val row = rowForColumn(axis).head
              frameSigmas(row) / math.abs(columns(axis)(row))
            }
          )
        else
          Left(
            OpError.InvalidScale(
              "anisotropic frame Gaussian is not separable on this rotated or sheared grid"
            )
          )

  private def unitScale(unit: LengthUnit): Double =
    unit match
      case LengthUnit.Meter      => 1.0
      case LengthUnit.Millimeter => 1.0e-3
      case LengthUnit.Micrometer => 1.0e-6

extension [
    S <: SampleSpace[?, ?],
    A,
    R <: AnyRank
](input: Sampled[S, A, Continuous, R])
  def gaussianBlur(
      sigma: SpatialSigma[input.sampleSpace.D],
      extent: FilterExtent[A] = FilterExtent.same(Border.reflect),
      truncate: Double = 3.0,
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      Dimension[input.sampleSpace.D],
      FilterOutput[A],
      FloatingDType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      A,
      R
    ]
  ] =
    Gaussian.blur(input, sigma, extent, truncate, policy)

  def gaussianBlurTo[B](
      sigma: SpatialSigma[input.sampleSpace.D],
      extent: FilterExtent[B] = FilterExtent.same(Border.reflect),
      truncate: Double = 3.0,
      policy: ExecutionPolicy = ExecutionPolicy()
  )(using
      Dimension[input.sampleSpace.D],
      NumericDType[A],
      FilterOutput[B],
      FloatingDType[B],
      ValueSemantics[B, Continuous]
  ): Either[
    OpError,
    ContinuousImage[
      ? <: SampleSpace[input.sampleSpace.F, input.sampleSpace.D],
      B,
      R
    ]
  ] =
    Gaussian.blurTo(input, sigma, extent, truncate, policy)
