# Ravel capability gate for production resampling

`AC-006` is green at the immutable Ravel revision
`52cea553b8b0e8f57ce000f32167ad56436da8a0`. The build resolves that revision
directly from the Ravel Git repository.

The revision provides the facilities required by the production kernel and
its representation court:

- rank-specific physical indexing for ranks one through four, including
  non-contiguous views;
- `NDArray.build`, whose callback fills one destination buffer that becomes the
  returned immutable array without an output-sized copy, using the explicit
  `ArrayBuilder.writeLinear` operation;
- allocation-free `CanonicalArray.readLinear` after a one-time checked
  refinement of a whole canonical array.

Ravel’s [CI run 30491768773](https://github.com/canardlapin/ravel/actions/runs/30491768773)
passed at the pinned SHA. Its cross-platform job covered JVM, Node, browser,
full-link, optimized JavaScript, formatting, MiMa scaffolding, representation,
and artifact inspection. Its NumPy-parity and documentation jobs also passed.

Reframe4s owns an independent compile-and-behavior probe in
`RavelCapabilitySuite`. It reads a reversed rank-three view with the
rank-specific accessor and writes the result through `NDArray.build`; the probe
passes on JVM and Scala.js.

## Production-kernel baseline

`ResamplingPerformanceSuite` runs a 192×192 affine linear plan on macOS 14.3
arm64 with OpenJDK 25.0.1. The focused local run at the pinned Ravel revision
produced:

| Measure | Value |
|---|---:|
| voxels | 36,864 |
| required value and validity payload | 589,824 B |
| total thread allocation | 597,520 B |
| allocation beyond output payload | 7,696 B |
| median throughput | 36.640 MVox/s |

The test permits at most 128 KiB beyond the two required output buffers. It
therefore gates both transient allocation and an upper bound on additional
live memory, not throughput alone. The plan structure reports zero materialized
coordinates, and four concurrent executions with distinct workspaces must
produce identical checksums. Timing is a same-machine development baseline, not
a cross-machine release claim.

The gate does not authorize a private replacement for Ravel. The production
plan uses Ravel’s public rank-specific readers and consuming builder directly.

## Lanczos-5 scan baseline

`MIG-424` extends the same production plan with normalized separable
Lanczos-5 interpolation. The kernel uses Ravel's rank-specific readers and the
existing primitive `scan` sink. Its mutable workspace owns three reusable
ten-element weight lanes; the voxel loop creates no coordinate or stencil
objects.

The recorded local D3 scan over 1,000 voxels produced:

| Measure | Value |
|---|---:|
| small-volume thread allocation | 464 B |
| large-volume thread allocation | 464 B |
| observed peak-heap delta | 0 B |
| median time | 1,866,125 ns |
| p95 time | 1,896,875 ns |
| median throughput | 0.535870 MVox/s |
| checksum | `817939.010651974800` |

Equal fixed allocation at the small and large shapes gates against per-voxel
object allocation. The test also requires zero materialized coordinates,
identical repeated checksums, and exact concurrent reuse with separate
workspaces. This is a local development baseline, not an external motion
superiority result.
