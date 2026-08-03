# Build and check the guide

The public guide source lives under `site-docs/`. The sbt project named `docs`
lives under `site/`, compiles examples on the JVM with mdoc, and renders the
site with Laika through `sbt-typelevel-site`.

## Generate the site

From the repository root:

```text
sbt -batch docs/tlSite
```

The generated site is under `site/target/docs/site/`. It is disposable build
output and must not be committed.

For interactive authoring:

```text
sbt docs/tlSitePreview
```

The preview is not a merge gate. Stop it when authoring finishes and run the
non-interactive task.

## Choose the right mdoc modifier

| Fence | Use |
| --- | --- |
| `scala mdoc` | Deterministic evaluated code whose output helps the reader |
| `scala mdoc:silent` | Compile and run setup without rendered output |
| `scala mdoc:fail` | Demonstrate an intentional compiler rejection |
| `scala mdoc:compile-only` | Typecheck side-effecting code without running it |

Examples should use public APIs, deterministic inputs, and explicit checked
failures. Do not require network access, credentials, machine-specific paths,
or external data during the default guide build.

## Validate the library around a documentation change

The minimum documentation gate is:

```text
sbt -batch docs/tlSite
git diff --check
git status --short
```

Build-definition changes also require the repository's canonical gates:

```text
sbt -batch compileAll
sbt -batch image4sTestAll
```

The full test alias covers JVM and Scala.js projections. The guide build itself
uses JVM module projections and must not be reported as Scala.js evidence.

## Work against sibling source checkouts

Ordinary builds use the exact Git revisions declared in `build.sbt`. Local
development can override them explicitly:

```text
sbt -batch \
  -Dimage4s.ravel.build=/absolute/path/to/ravel \
  -Dimage4s.gale.build=/absolute/path/to/gale \
  -Dimage4s.locus4s.build=/absolute/path/to/locus4s \
  -Dimage4s.intaglio.build=/absolute/path/to/intaglio \
  docs/tlSite
```

Record every override in verification notes. A passing sibling-source build is
useful integration evidence, but it does not replace the clean pinned-source
gate.

## Inspect the rendered product

After `docs/tlSite`, inspect more than the exit code:

- landing-page routes and navigation order;
- orphan or duplicate pages;
- internal and external links;
- code wrapping and table layout at desktop and narrow widths;
- generated paths and search indexes for internal planning material; and
- the final working tree for generated output or unrelated changes.

The site is published by the repository's Pages workflow. Local generation is
not deployment evidence; verify the workflow and served URL separately when a
publication change is in scope.
