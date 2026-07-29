# Image representation evidence

This is the evidence ledger for canonical epic
`bd-01KYNR748JZYEP3A5Q1H9ZB3AR`, including contract child
`bd-01KYNR7RR9DB7SK5SHMJZBCJGW` and image4s implementation child
`bd-01KYNR806KDX4GFH3B486AM1CG`. The normative rules are in
[the canonical image representation contract](image-representation-contract.md).

## Audited sources

| Source | Revision | Relevant fact |
|---|---|---|
| image4s | `5b3e63cea01b48a67758f10cdb5d0cc80fc7c507` | initial independent repository commit; `Sampled` owns one immutable Ravel value and validates `grid.shape ++ nonSpatialAxes.shape` |
| Ravel | `f804ba51242aae3a1442b3855a20bd896ffa8b64` | canonical storage is C order; checked canonical linear access and rank-specific access are allocation-free; immutable strided views preserve logical indexing |
| ScalaFIM | `6b5d4993d9acc59873e00eed5211b56728968a78` | private `NDArray.linearIndex` is first-axis-fastest; `NeuroVol` and `NeuroVec` delegate `(i,j,k[,t])` to it |

GitHub Actions run
[`30500249009`](https://github.com/canardlapin/image4s/actions/runs/30500249009)
passed the standalone JVM, Scala.js, Node NIfTI, representation, performance,
and optimized-link gates for the image4s commit above.

## Required signatures

Every workload must agree on:

- logical sample count;
- ordinary sum;
- a coordinate-derived weighted sum that changes under a transpose;
- the declared logical shape;
- the expected storage-layout classification.

A timing row is invalid if any signature differs. Raw timings from different
machines, runtimes, traversal orders, or result layouts are not compared as a
speed ratio.

## Commands

```text
sbt -batch "image4s-lawsJVM/testOnly image4s.laws.ImageRepresentationContractSuite"
sbt -batch "image4s-lawsJS/testOnly image4s.laws.ImageRepresentationContractSuite"
sbt -Dimage4s.revision=<git-sha> -batch "image4s-lawsJVM/testOnly image4s.laws.ImageRepresentationPerformanceSuite"
sbt -batch "image4s-coreJS/Test/fullOptJS"
sbt -J-Xmx4G -batch testAll
```

## Pre-change baseline

The focused JVM court passed on an Apple ARM64 macOS host. sbt reported its
Homebrew JDK as 25.0.1. One hundred warm-up traversals preceded seven
allocation samples and twenty-one timing samples. The table reports the range
of per-run medians across two focused repeats and one complete image4s
core/laws run in the same sbt process. Every row visited 22,440 samples and
produced sum `58,153,260` and weighted sum `653,967,300,580`.

| Workload | Allocation | Median | ns/sample |
|---|---:|---:|---:|
| legacy first-axis buffer, C-order coordinates | 40 B | 16,750–17,292 ns | 0.746–0.771 |
| direct ranked Ravel, C-order coordinates | 40 B | 16,458–17,542 ns | 0.733–0.782 |
| checked `Sampled.valueAt`, C-order coordinates | 30,697,960–41,828,200 B | 2,480,167–3,147,500 ns | 110.524–140.263 |
| legacy first-axis buffer, volume order | 40 B | 19,250 ns | 0.858 |
| direct ranked Ravel, volume order | 40 B | 19,625–19,875 ns | 0.875–0.886 |
| legacy first-axis buffer, z-slice order | 40 B | 19,209–19,250 ns | 0.856–0.858 |
| direct ranked Ravel, z-slice order | 40 B | 19,625–19,875 ns | 0.875–0.886 |

The legacy rows are an independent raw-buffer oracle implementing the audited
formula. They are not a claim about the full public ScalaFIM wrapper. The
direct Ravel rows establish that ranked storage access is effectively
allocation-free in every traversal order. The checked dynamic
`Sampled.valueAt` path visibly allocates about 1.34–1.82 KiB per sample and is
about 150–191 times slower than direct Ravel in the matched C-order row. This
loss is
accepted only as the pre-change baseline for
`bd-01KYNR806KDX4GFH3B486AM1CG`; it is not an acceptable final image-access
path.

The checked-path allocation varies with JVM optimization state even after
warm-up, so its range—not a favorable single run—is the admitted baseline.
Direct Ravel allocation and timings remained stable. None of the raw legacy
rows is a performance admission threshold.

The first draft of the court routed reads through `Function4` and was rejected
after it attributed 538,616 bytes of boxing allocation to direct Ravel.
Inlining the read function removed the harness artifact and the rerun above is
the admitted receipt.

These are matched local development numbers from the implementation snapshot
identified above. They are not a cross-runtime speed claim.

## Post-change receipt

After adding statically ranked `Sampled.apply` and routing common checked ranks
directly to Ravel, two fully warmed focused repeats produced the following
matched C-order rows:

| Workload | Allocation | ns/sample |
|---|---:|---:|
| direct ranked Ravel | 40 B | 0.733–0.797 |
| ranked `Sampled(i,j,k,t)` | 40 B | 0.733–0.737 |
| checked `Sampled.valueAt` | 23,158,120 B | 66.735–69.107 |

All three rows visited 22,440 samples and retained the same ordinary and
weighted checksums as the pre-change court. Ranked `Sampled` adds no measured
allocation or wrapper penalty relative to direct Ravel. The checked dynamic
method still allocates about 1.01 KiB per call because callers supply immutable
index vectors and receive an `Either`; it remains the validation API, not the
hot-loop API. Its common rank-2, rank-3, and rank-4 branches no longer
concatenate vectors or construct an `IArray`.

The JVM and Scala.js core suites also prove that:

- `selectTime`, `selectChannel`, and `selectDirection` remove the declared axis
  and return a Ravel view whose backing buffer is larger than the view;
- `spatialView` returns a non-whole-buffer crop and shifts the full affine so
  the selected source coordinate becomes the new grid origin;
- `canonicalLayout` copies only a non-canonical layout, while
  `materializedCopy` always creates a new canonical buffer;
- dynamically ranked storage must pass `requireDataRank` before ranked access
  or repeated rank drops.

The standalone image4s repository gate passes 157 tests: geometry 18/18,
core 22/22, reference 11/11, locus bridge 9/9, NIfTI 23 JVM plus 5 Node, and
laws 5 JVM plus 4 Scala.js. The independently scheduled JVM performance row
again measured 40 bytes for both direct Ravel and ranked `Sampled`, with
identical checksums. The checked dynamic row retains its visible allocation
loss rather than hiding JVM-state variance.

The production-optimized Scala.js test link also succeeded. Inspection of its
rank-2, rank-3, and rank-4 assertions showed the inline `Sampled.apply`
expanding to the corresponding Ravel rank-specific layout-index operation and
storage probe. No `Vector`, `IArray`, `Either`, or intermediate image wrapper
was present between the sampled value's `data` field and the Ravel read.

The cross-repository reframe4s architecture courts also pass with image4s as
an external immutable dependency: 27 nodes, 83 edges, an acyclic graph, and 56
canonical owners across 568 public qualified names.

## Current cross-repository removal audit

The structural audit was added after the image4s receipt:

```text
node scripts/verify-image-unification.mjs \
  --scalafim /Users/bbuchsbaum/code/scala/scalafim \
  --report-only
```

On 2026-07-29 the non-reporting command scanned 1,077 Scala files and passed
with no raw access, parallel array/image/geometry owner, duplicate dense-field
kernel, or mutable source-composite finding. ScalaFIM now pins image4s directly
to an immutable Git revision. Its aggregate consumer rerun remains coordinated
with the independently tracked graph/locus source-build cutover.
