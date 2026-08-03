# The sampled-image model

An image4s value contains the samples and the information needed to interpret
them. Code that receives the image can inspect its spatial grid, declared
non-spatial axes, metadata, and semantic role without consulting a separate
side table.

```text
Sampled image
├── Ravel values
├── sample space
│   ├── spatial grid
│   └── ordered non-spatial axes
├── metadata
└── semantic role
```

## Values come from Ravel

A Ravel `NDArray` owns dense storage, rank, shape, layout, strides, and storage
views. image4s does not copy that machinery into a second array hierarchy. It
adds the sampling information that turns array entries into image samples.

The logical array shape has a fixed order:

```text
spatial grid extents ++ ordered non-spatial axis extents
```

A `6 × 5` grid sampled at three times therefore uses an array with logical
shape `6 × 5 × 3`.

## The sample space says where samples belong

A `SampleSpace` binds one spatial `Grid` to an ordered `NonSpatialAxes` value.
The grid maps integer lattice indices into a named coordinate frame. Each
non-spatial `Axis` declares the coordinate attached to its samples, such as
time in seconds or a channel label.

This is why equal array shapes are insufficient for pointwise arithmetic. Two
`100 × 100` arrays can cover different physical locations or pair their
trailing samples with different times.

## Metadata travels with the image

`ImageMetadata` carries descriptive entries through operations that preserve
the image's interpretation. Metadata does not replace geometry or axes. A
string field cannot grant permission to combine two grids or reinterpret a
categorical image as continuous.

## Semantic roles constrain operations

Continuous, categorical, and mask images use the same sampled representation
but expose different valid operations.

| Role | Meaning | Typical operation |
| --- | --- | --- |
| Continuous | Values represent an interpolation-compatible field | Gaussian blur or gradient |
| Categorical | Numeric or textual codes name classes | Label palette or exact selection |
| Mask | Boolean membership over the sample space | Dilation or erosion |

The role is separate from storage dtype. Integer-backed data can be continuous,
but a filter must choose a floating output instead of truncating computed
values. Numeric label codes remain categorical even though arithmetic exists
for their storage type.

## The full type records all four parts

The concrete representation is:

```scala
Sampled[S, A, Sem, R]
```

| Parameter | Records |
| --- | --- |
| `S` | The exact `SampleSpace` type |
| `A` | Stored element type |
| `Sem` | Continuous, categorical, or mask semantics |
| `R` | Ravel array rank |

Most application code should not write this type. Constructors such as
`Image.continuous`, `Image.categorical`, and `Image.mask` infer it. The type
parameters become useful when a library function must state exactly which
images it accepts or preserves.

## What an operation must account for

When reading an image4s result, ask four separate questions:

1. Were values selected, copied, converted, or newly computed?
2. Did the spatial grid stay the same or acquire a derived affine?
3. Were non-spatial axes preserved, removed, or reordered?
4. Did the semantic role stay the same or change explicitly?

The task guides answer these questions after each major workflow. Next, read
[Geometry and sampled axes](geometry-and-axes.md) for the concrete coordinate
objects, or start using them in [exact views](../work/exact-views.md).
