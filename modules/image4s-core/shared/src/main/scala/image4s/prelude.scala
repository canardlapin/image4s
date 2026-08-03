package image4s

/** Curated imports for ordinary image construction and geometry.
  *
  * Data containers and feature modules remain explicit: import `ravel.NDArray`, filtering and
  * operations, NIfTI, Intaglio, locus, and reframe4s from their owning packages.
  */
object prelude:
  export image4s.{
    AxesSpec,
    Axis,
    AxisCoordinate,
    AxisKind,
    AxisSpec,
    AxisUnit,
    Categorical,
    CategoricalImage,
    Continuous,
    ContinuousImage,
    DoubleContinuousImage,
    FloatContinuousImage,
    FrameSpec,
    GridSpec,
    Image,
    ImageError,
    ImageMetadata,
    Mask,
    MaskImage,
    NonSpatialAxes,
    SampleSpace,
    Sampled,
    SamplingSpec,
    SomeSampled,
    ValueSemantics
  }
  export image4s.geometry.{
    Affine,
    ContinuousIndex,
    CoordinateConvention,
    D2,
    D3,
    Dim,
    Dimension,
    Frame,
    GeometryError,
    Grid,
    Index,
    LatticeIndex,
    LengthUnit,
    Point,
    Vec
  }
  export image4s.apply
  export ravel.DType.given
