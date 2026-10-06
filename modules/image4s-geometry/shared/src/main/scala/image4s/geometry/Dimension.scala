package image4s.geometry

/** Compatibility names backed by the sole spatial4s dimension authority. */
type Dim = spatial4s.Dim
type D2 = spatial4s.D2
type D3 = spatial4s.D3
type Dimension[D <: Dim] = spatial4s.Dimension[D]
object Dimension:
  export spatial4s.Dimension.{apply, given}
