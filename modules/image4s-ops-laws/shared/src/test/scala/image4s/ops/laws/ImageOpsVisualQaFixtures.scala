package image4s.ops.laws

import _root_.intaglio.DisplayWindow
import _root_.intaglio.RasterImage
import image4s.Continuous
import image4s.ImageError
import image4s.MaskImage
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.filter.Gradient
import image4s.filter.LinearFilter
import image4s.filter.gaussianBlur
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.intaglio.DisplayBridge
import image4s.intaglio.DisplayPlan
import image4s.intaglio.LabelPalette
import image4s.intaglio.SliceAxis
import image4s.morphology.StructuringElement
import image4s.morphology.close
import image4s.morphology.dilate
import image4s.morphology.erode
import image4s.morphology.open
import image4s.morphology.threshold
import image4s.ops.Border
import image4s.ops.Convolution
import image4s.ops.Correlation
import image4s.ops.FilterExtent
import image4s.ops.IndexCoordinates
import image4s.ops.Kernel
import image4s.ops.OpError
import image4s.ops.Offset
import image4s.ops.Radius
import image4s.ops.SpatialSigma
import image4s.ops.Support
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.map

/** Shared image-operation fixtures used by both cross-platform tests and the
  * JVM visual-review gallery. Every raster is lowered through DisplayBridge.
  */
object ImageOpsVisualQaFixtures:
  final case class VisualCase(name: String, raster: RasterImage)

  def build(): Vector[VisualCase] =
    val source2D = pipelineSource2D
    val sigma2D = opsRight(SpatialSigma.samples[D2](1.0))
    val blurred2D = opsRight(source2D.gaussianBlur(sigma2D))
    val sobel2D = opsRight(Gradient.sobel(blurred2D, IndexCoordinates))
    val scharr2D = opsRight(Gradient.scharr(blurred2D, IndexCoordinates))
    val thresholded2D = opsRight(source2D.threshold(5.0))
    val cross2D =
      StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val box2D = StructuringElement.box[D2](opsRight(Radius.samples(1)))
    val disk2D = StructuringElement.disk[D2](opsRight(Radius.samples(2)))
    val eroded2D = opsRight(thresholded2D.erode(cross2D))
    val dilated2D = opsRight(thresholded2D.dilate(cross2D))
    val opened2D = opsRight(thresholded2D.open(cross2D))
    val closed2D = opsRight(thresholded2D.close(cross2D))
    val boxDilated2D = opsRight(thresholded2D.dilate(box2D))
    val diskDilated2D = opsRight(thresholded2D.dilate(disk2D))

    val impulse2D = continuous2D("visual-impulse", 9) { (row, column) =>
      if row == 4 && column == 4 then 1.0 else 0.0
    }
    val impulseBlurred2D = opsRight(impulse2D.gaussianBlur(sigma2D))
    val gaussianValid2D =
      opsRight(impulse2D.gaussianBlur(sigma2D, FilterExtent.valid))
    val gaussianFull2D =
      opsRight(
        impulse2D.gaussianBlur(
          sigma2D,
          FilterExtent.full(Border.Constant(0.0))
        )
      )
    val asymmetricSupport =
      opsRight(
        Support.create[D2](
          Vector(
            Offset.unsafe[D2](Vector(-1, 0)),
            Offset.unsafe[D2](Vector(0, 0)),
            Offset.unsafe[D2](Vector(1, 0))
          )
        )
      )
    val asymmetricKernel =
      opsRight(Kernel.sparse(asymmetricSupport, Vector(1.0, 3.0, 7.0)))
    val asymmetricExtent = FilterExtent.same(Border.Constant(0.0))
    val correlated2D =
      opsRight(
        LinearFilter.correlate(
          impulse2D,
          Correlation[D2, Double](asymmetricKernel, asymmetricExtent)
        )
      )
    val convolved2D =
      opsRight(
        LinearFilter.convolve(
          impulse2D,
          Convolution[D2, Double](asymmetricKernel, asymmetricExtent)
        )
      )

    val source3D = pipelineSource3D
    val sigma3D = opsRight(SpatialSigma.samples[D3](1.0))
    val blurred3D = opsRight(source3D.gaussianBlur(sigma3D))
    val sobel3D = opsRight(Gradient.sobel(blurred3D, IndexCoordinates))
    val scharr3D = opsRight(Gradient.scharr(blurred3D, IndexCoordinates))
    val thresholded3D = opsRight(source3D.threshold(5.0))
    val ball3D = StructuringElement.ball[D3](opsRight(Radius.samples(1)))
    val dilated3D = opsRight(thresholded3D.dilate(ball3D))
    val closed3D = opsRight(thresholded3D.close(ball3D))

    val scalarPlan =
      DisplayPlan(DisplayWindow.unsafe(0.0, 10.0))
    val gradientPlan =
      DisplayPlan(DisplayWindow.unsafe(-5.0, 5.0))
    val impulsePlan =
      DisplayPlan(DisplayWindow.unsafe(0.0, 1.0))
    val asymmetricPlan =
      DisplayPlan(DisplayWindow.unsafe(0.0, 8.0))
    val maskPlan =
      DisplayPlan(DisplayWindow.unsafe(0.0, 1.0))

    Vector(
      VisualCase("d2-source", DisplayBridge.renderRaster(source2D, scalarPlan)),
      VisualCase(
        "d2-gaussian",
        DisplayBridge.renderRaster(blurred2D, scalarPlan)
      ),
      VisualCase(
        "d2-sobel-x",
        DisplayBridge.renderRaster(sobel2D.components(0), gradientPlan)
      ),
      VisualCase(
        "d2-sobel-y",
        DisplayBridge.renderRaster(sobel2D.components(1), gradientPlan)
      ),
      VisualCase(
        "d2-scharr-x",
        DisplayBridge.renderRaster(scharr2D.components(0), gradientPlan)
      ),
      VisualCase(
        "d2-scharr-y",
        DisplayBridge.renderRaster(scharr2D.components(1), gradientPlan)
      ),
      VisualCase(
        "d2-correlation-asymmetric",
        DisplayBridge.renderRaster(correlated2D, asymmetricPlan)
      ),
      VisualCase(
        "d2-convolution-asymmetric",
        DisplayBridge.renderRaster(convolved2D, asymmetricPlan)
      ),
      VisualCase(
        "d2-gaussian-valid",
        DisplayBridge.renderRaster(gaussianValid2D, impulsePlan)
      ),
      VisualCase(
        "d2-gaussian-full",
        DisplayBridge.renderRaster(gaussianFull2D, impulsePlan)
      ),
      VisualCase("d2-threshold", renderMask(thresholded2D)),
      VisualCase("d2-erode", renderMask(eroded2D)),
      VisualCase("d2-dilate", renderMask(dilated2D)),
      VisualCase("d2-opening", renderMask(opened2D)),
      VisualCase("d2-closing", renderMask(closed2D)),
      VisualCase("d2-box-dilate", renderMask(boxDilated2D)),
      VisualCase("d2-disk-dilate", renderMask(diskDilated2D)),
      VisualCase(
        "d2-gaussian-impulse",
        DisplayBridge.renderRaster(impulseBlurred2D, impulsePlan)
      ),
      VisualCase(
        "d3-source-x",
        renderSlice(source3D, SliceAxis.X, 3, scalarPlan)
      ),
      VisualCase(
        "d3-source-y",
        renderSlice(source3D, SliceAxis.Y, 3, scalarPlan)
      ),
      VisualCase(
        "d3-source-z",
        renderSlice(source3D, SliceAxis.Z, 2, scalarPlan)
      ),
      VisualCase(
        "d3-gaussian-z",
        renderSlice(blurred3D, SliceAxis.Z, 2, scalarPlan)
      ),
      VisualCase(
        "d3-sobel-z",
        renderSlice(sobel3D.components(0), SliceAxis.Z, 2, gradientPlan)
      ),
      VisualCase(
        "d3-scharr-z",
        renderSlice(scharr3D.components(0), SliceAxis.Z, 2, gradientPlan)
      ),
      VisualCase(
        "d3-threshold-z",
        renderMaskSlice(thresholded3D, SliceAxis.Z, 2, maskPlan)
      ),
      VisualCase(
        "d3-ball-dilate-z",
        renderMaskSlice(dilated3D, SliceAxis.Z, 2, maskPlan)
      ),
      VisualCase(
        "d3-ball-close-z",
        renderMaskSlice(closed3D, SliceAxis.Z, 2, maskPlan)
      )
    )

  private def pipelineSource2D =
    continuous2D("visual-pipeline", 15) { (row, column) =>
      val plateau =
        row >= 4 && row <= 10 && column >= 4 && column <= 10 &&
          !(row == 7 && column == 7)
      val speck = row == 2 && column == 12
      if plateau || speck then 10.0 else 0.0
    }

  private def pipelineSource3D =
    continuous3D("visual-pipeline-3d", Vector(7, 6, 5)) {
      (x, y, z) =>
        val plateau =
          x >= 2 && x <= 4 && y >= 2 && y <= 4 && z >= 1 && z <= 3 &&
            !(x == 3 && y == 3 && z == 2)
        val speck = x == 1 && y == 1 && z == 4
        if plateau || speck then 10.0 else 0.0
    }

  private def continuous2D(
      label: String,
      size: Int
  )(values: (Int, Int) => Double) =
    val frame = geometryRight(Frame.named[D2](label))
    val grid = geometryRight(Grid.in(frame)(Vector(size, size), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[2]](
        space,
        NDArray.tabulate[Double](size, size)(values)
      )
    )

  private def continuous3D(
      label: String,
      shape: Vector[Int]
  )(values: (Int, Int, Int) => Double) =
    val frame = geometryRight(Frame.named[D3](label))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D3]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[3]](
        space,
        NDArray.tabulate[Double](shape(0), shape(1), shape(2))(values)
      )
    )

  private def renderMask[S <: SampleSpace[?, D2]](
      mask: MaskImage[S, Rank[2]]
  ): RasterImage =
    val labels =
      imageRight(
        Sampled.categorical[Int, Rank[2]](
          mask.sampleSpace,
          mask.data.map(value => if value then 1 else 0),
          mask.metadata
        )
      )
    DisplayBridge.renderLabels(labels, LabelPalette())

  private def renderSlice[S <: SampleSpace[?, D3]](
      image: Sampled[S, Double, Continuous, Rank[3]],
      axis: SliceAxis,
      index: Int,
      plan: DisplayPlan
  ): RasterImage =
    bridgeRight(DisplayBridge.renderSliceRaster(image, axis, index, plan))

  private def renderMaskSlice[S <: SampleSpace[?, D3]](
      mask: MaskImage[S, Rank[3]],
      axis: SliceAxis,
      index: Int,
      plan: DisplayPlan
  ): RasterImage =
    val values =
      imageRight(
        Sampled.continuous[Double, Rank[3]](
          mask.sampleSpace,
          mask.data.map(value => if value then 1.0 else 0.0),
          mask.metadata
        )
      )
    renderSlice(values, axis, index, plan)

  private def bridgeRight[A](
      value: Either[image4s.intaglio.DisplayBridgeError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => throw new IllegalStateException(error.message)

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
