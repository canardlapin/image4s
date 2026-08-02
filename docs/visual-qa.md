# Image-operation visual QA

The operation modules have a cross-platform visual contract in
`image4s.ops.laws.ImageOpsVisualQaSuite`. It lowers deterministic D2 and D3
fixtures through the production `image4s-intaglio` display bridge. The suite
currently contains 27 raster cases:

- D2 source and Gaussian stages, Sobel and Scharr components, primitive
  correlation/convolution orientation, and Gaussian `Valid`/`Full` extents;
- D2 threshold, erosion, dilation, opening, closing, and box/cross/disk
  structuring-element silhouettes;
- D3 source slices on all three orthogonal axes, Gaussian and gradient slices,
  thresholding, and ball dilation/closing slices;
- deterministic raster dimensions, stage names, and repeated-build equality.

The asymmetric impulse cases distinguish correlation from convolution. The D3
cases use a non-cubic source grid so an axis-order or slice-dimension mistake
cannot hide behind a square image.

Run the normal cross-platform ops-law tests with:

```sh
sbt "image4s-ops-lawsJVM / Test / test"
sbt "image4s-ops-lawsJS / Test / test"
```

For human inspection, generate PNGs and an HTML index with:

```sh
sbt imageOpsVisualQaJVM
open target/image-ops-visual-qa/index.html
```

The gallery is review evidence, not a replacement for numerical laws,
differential tests, metamorphic checks, or performance courts. It does not
claim to cover unsupported branches such as FFT filtering, and it does not
prove scientific parity against an external implementation. Its PNGs are
nearest-neighbour presentations of the deterministic fixtures, generated under
`target/`, and are intentionally not committed as golden files.
