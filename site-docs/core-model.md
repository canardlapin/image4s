# The core model

image4s has one dense sampled-image owner: `Sampled`. It wraps a Ravel
`NDArray` together with the complete `SampleSpace` that gives those values
meaning.

| Part | Owns |
| --- | --- |
| Ravel `NDArray` | dense storage, rank, layout, strides, and storage views |
| `Grid` | spatial shape, frame, affine index-to-frame mapping, and grid identity |
| `Axis` / `NonSpatialAxes` | ordered time, channel, echo, direction, categorical, or custom sampling coordinates |
| `Sampled` | the one value owner that binds storage, space, metadata, and value semantics |

The logical shape is always the spatial grid shape followed by the declared
non-spatial axis shapes. A time series with a `6 × 7 × 5` grid and four time
points therefore has logical shape `Vector(6, 7, 5, 4)`.

## Value semantics are part of the type

Continuous, categorical, and mask images are semantic views over the same
`Sampled` representation. They are not separate array containers. A continuous
operation can require interpolation-compatible values; a categorical operation
does not silently turn labels into a continuous field; and a mask remains a
mask rather than merely a Boolean array with lost spatial context.

The semantic role is intentionally separate from storage dtype. A `Double`
can be continuous, but a numeric storage conversion still requires an explicit
policy. Integer-backed continuous values need an explicit floating output when
an operation such as Gaussian filtering would otherwise lose precision.

## Axes are declarations, not trailing integers

Use `Axis.regular` when coordinates follow an origin and step, `Axis.explicit`
when the coordinate values are individually supplied, and
`Axis.categorical` for labels. Selecting an axis by its declared kind is safer
than assuming that a particular trailing position is time:

```scala
val volume = image.atTime(2)
```

If the image has no unique time axis, `atTime` returns an error. The same rule
applies to `selectChannel`, `selectDirection`, and the general
`selectAxis` operation.

## Ownership and alignment

Two images constructed from the same live `SampleSpace` can be combined
without a new proof. Independently reconstructed spaces may look identical but
do not silently share runtime ownership. Use an explicit `SamplingAlignment`
when a workflow has established that relation.

This distinction protects a common scientific failure mode: values that have
the same shape but belong to different physical frames, affine grids, or
sampling coordinates.

For the longer normative contract, see the repository's
[canonical image representation contract](https://github.com/canardlapin/image4s/blob/main/docs/image-representation-contract.md).
