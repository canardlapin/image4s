import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.{CrossProject, CrossType}
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

ThisBuild / organization := "io.github.canardlapin"
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / versionPolicyIntention := Compatibility.None
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some(
  "^\\d+\\.\\d+\\.\\d+\\+\\d+".r
)
ThisBuild / homepage := Some(url("https://github.com/canardlapin/image4s"))
ThisBuild / description :=
  "Typed multidimensional images and image operations for Scala 3 on the JVM and Scala.js."
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/canardlapin/image4s"),
    "scm:git:https://github.com/canardlapin/image4s.git",
    Some("scm:git:git@github.com:canardlapin/image4s.git")
  )
)
ThisBuild / licenses := List(
  "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
)
ThisBuild / developers := List(
  Developer(
    id = "canardlapin",
    name = "canardlapin",
    email = "307091466+canardlapin@users.noreply.github.com",
    url = url("https://github.com/canardlapin")
  )
)
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Werror"
)
ThisBuild / Test / parallelExecution := false
Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)

lazy val ravelRevision = "a9237849af96218588693eaf4db64569760f9be5"
lazy val ravelBuild =
  sys.props
    .get("image4s.ravel.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/ravel.git#$ravelRevision"))
lazy val ravelCoreJVM = ProjectRef(ravelBuild, "coreJVM")
lazy val ravelCoreJS = ProjectRef(ravelBuild, "coreJS")
lazy val ravelStencilJVM = ProjectRef(ravelBuild, "stencilJVM")
lazy val ravelStencilJS = ProjectRef(ravelBuild, "stencilJS")
lazy val ravelPackedJVM = ProjectRef(ravelBuild, "packedJVM")
lazy val ravelPackedJS = ProjectRef(ravelBuild, "packedJS")

lazy val galeRevision = "f869613cec0a89e57b6c995b0a02cf471ac7127c"
lazy val galeBuild =
  sys.props
    .get("image4s.gale.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/gale.git#$galeRevision"))
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS = ProjectRef(galeBuild, "coreJS")

lazy val locus4sRevision = "1187276933605a32d50342624a4e6391a6bbfd5f"
lazy val locus4sBuild =
  sys.props
    .get("image4s.locus4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(
      uri(s"https://github.com/canardlapin/locus4s.git#$locus4sRevision")
    )
lazy val locus4sCoreJVM = ProjectRef(locus4sBuild, "locus4s-coreJVM")
lazy val locus4sCoreJS = ProjectRef(locus4sBuild, "locus4s-coreJS")
lazy val locus4sDataJVM = ProjectRef(locus4sBuild, "locus4s-dataJVM")
lazy val locus4sDataJS = ProjectRef(locus4sBuild, "locus4s-dataJS")

lazy val intaglioRevision = "596b398af380079e4b251535230d0bc03cd88c51"
lazy val intaglioBuild =
  sys.props
    .get("image4s.intaglio.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(
      uri(s"https://github.com/canardlapin/intaglio.git#$intaglioRevision")
    )
lazy val intaglioCoreJVM = ProjectRef(intaglioBuild, "coreJVM")
lazy val intaglioCoreJS = ProjectRef(intaglioBuild, "coreJS")

lazy val sharedSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.3.0" % Test,
    "org.scalameta" %%% "munit-scalacheck" % "1.3.0" % Test
  )
)

def imageProject(artifact: String): CrossProject =
  CrossProject(artifact, file(s"modules/$artifact"))(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .settings(sharedSettings)
    .settings(
      name := artifact,
      description :=
        "Typed multidimensional image functionality for Scala 3 on the JVM and Scala.js."
    )
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    )

lazy val image4sGeometry =
  imageProject("image4s-geometry")
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))

lazy val image4sCore =
  imageProject("image4s-core")
    .dependsOn(image4sGeometry)
    .jvmConfigure(_.dependsOn(ravelCoreJVM, ravelPackedJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS, ravelPackedJS))

lazy val image4sNifti =
  imageProject("image4s-nifti")
    .dependsOn(image4sCore, image4sGeometry)
    .jvmConfigure(_.dependsOn(ravelCoreJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS))

lazy val image4sReference =
  imageProject("image4s-reference")
    .dependsOn(image4sCore)
    .jvmConfigure(_.dependsOn(ravelCoreJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS))

lazy val image4sLaws =
  imageProject("image4s-laws")
    .dependsOn(image4sCore, image4sReference)

lazy val image4sLocus =
  imageProject("image4s-locus")
    .dependsOn(image4sCore, image4sGeometry)
    .jvmConfigure(_.dependsOn(locus4sCoreJVM, locus4sDataJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS, locus4sDataJS))

lazy val image4sIntaglio =
  imageProject("image4s-intaglio")
    .dependsOn(image4sCore, image4sGeometry)
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .jsConfigure(_.dependsOn(intaglioCoreJS))

// Temporarily hosted here for extractability into canardlapin/image4s-ops.
// Permanent rule: image4s-core must never depend on these modules.
lazy val image4sOpsCore =
  imageProject("image4s-ops-core")
    .dependsOn(image4sCore)
    .jvmConfigure(_.dependsOn(ravelCoreJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS))

lazy val image4sFilter =
  imageProject("image4s-filter")
    .dependsOn(image4sOpsCore, image4sCore)
    .jvmConfigure(_.dependsOn(ravelCoreJVM, ravelStencilJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS, ravelStencilJS))

lazy val image4sMorphology =
  imageProject("image4s-morphology")
    .dependsOn(image4sOpsCore, image4sCore)
    .jvmConfigure(_.dependsOn(ravelCoreJVM, ravelStencilJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS, ravelStencilJS))

lazy val image4sOpsLaws =
  imageProject("image4s-ops-laws")
    .dependsOn(
      image4sOpsCore,
      image4sFilter,
      image4sMorphology,
      image4sLaws,
      image4sIntaglio % "test->compile"
    )
    .settings(publish / skip := true)

lazy val root =
  project
    .in(file("."))
    .aggregate(
      image4sGeometry.jvm,
      image4sGeometry.js,
      image4sCore.jvm,
      image4sCore.js,
      image4sNifti.jvm,
      image4sNifti.js,
      image4sReference.jvm,
      image4sReference.js,
      image4sLaws.jvm,
      image4sLaws.js,
      image4sLocus.jvm,
      image4sLocus.js,
      image4sIntaglio.jvm,
      image4sIntaglio.js,
      image4sOpsCore.jvm,
      image4sOpsCore.js,
      image4sFilter.jvm,
      image4sFilter.js,
      image4sMorphology.jvm,
      image4sMorphology.js,
      image4sOpsLaws.jvm,
      image4sOpsLaws.js
    )
    .settings(
      name := "image4s-root",
      publish / skip := true
    )

lazy val docs =
  project
    .in(file("site"))
    .dependsOn(
      image4sCore.jvm,
      image4sFilter.jvm,
      image4sMorphology.jvm,
      image4sNifti.jvm,
      image4sIntaglio.jvm
    )
    .enablePlugins(org.typelevel.sbt.TypelevelSitePlugin)
    .settings(
      name := "image4s-site",
      publish / skip := true,
      mdocIn := (ThisBuild / baseDirectory).value / "site-docs",
      tlSitePublishBranch := None,
      tlSitePublishTags := false
    )

lazy val image4sCompileAll =
  taskKey[Unit]("Compile every image4s JVM and Scala.js module")
image4sCompileAll := {
  (image4sGeometry.jvm / Compile / compile).value
  (image4sGeometry.js / Compile / compile).value
  (image4sCore.jvm / Compile / compile).value
  (image4sCore.js / Compile / compile).value
  (image4sNifti.jvm / Compile / compile).value
  (image4sNifti.js / Compile / compile).value
  (image4sReference.jvm / Compile / compile).value
  (image4sReference.js / Compile / compile).value
  (image4sLaws.jvm / Compile / compile).value
  (image4sLaws.js / Compile / compile).value
  (image4sLocus.jvm / Compile / compile).value
  (image4sLocus.js / Compile / compile).value
  (image4sIntaglio.jvm / Compile / compile).value
  (image4sIntaglio.js / Compile / compile).value
  (image4sOpsCore.jvm / Compile / compile).value
  (image4sOpsCore.js / Compile / compile).value
  (image4sFilter.jvm / Compile / compile).value
  (image4sFilter.js / Compile / compile).value
  (image4sMorphology.jvm / Compile / compile).value
  (image4sMorphology.js / Compile / compile).value
  (image4sOpsLaws.jvm / Compile / compile).value
  (image4sOpsLaws.js / Compile / compile).value
  ()
}
addCommandAlias(
  "makeReleasePoms",
  ";image4s-geometryJVM/makePom;image4s-geometryJS/makePom;image4s-coreJVM/makePom;image4s-coreJS/makePom;image4s-niftiJVM/makePom;image4s-niftiJS/makePom;image4s-referenceJVM/makePom;image4s-referenceJS/makePom;image4s-lawsJVM/makePom;image4s-lawsJS/makePom;image4s-locusJVM/makePom;image4s-locusJS/makePom;image4s-intaglioJVM/makePom;image4s-intaglioJS/makePom;image4s-ops-coreJVM/makePom;image4s-ops-coreJS/makePom;image4s-filterJVM/makePom;image4s-filterJS/makePom;image4s-morphologyJVM/makePom;image4s-morphologyJS/makePom"
)
addCommandAlias(
  "imageOpsVisualQaJVM",
  "image4s-ops-lawsJVM / Test / runMain image4s.ops.laws.ImageOpsVisualQa"
)
addCommandAlias(
  "imageOpsParityJVM",
  "image4s-ops-lawsJVM / Test / runMain image4s.ops.laws.ImageOpsParityBenchmark"
)
addCommandAlias(
  "image4sTestAll",
  ";image4s-geometryJVM/test;image4s-geometryJS/test;image4s-coreJVM/test;image4s-coreJS/test;image4s-niftiJVM/test;image4s-niftiJS/test;image4s-referenceJVM/test;image4s-referenceJS/test;image4s-lawsJVM/test;image4s-lawsJS/test;image4s-locusJVM/test;image4s-locusJS/test;image4s-intaglioJVM/test;image4s-intaglioJS/test;image4s-ops-coreJVM/test;image4s-ops-coreJS/test;image4s-filterJVM/test;image4s-filterJS/test;image4s-morphologyJVM/test;image4s-morphologyJS/test;image4s-ops-lawsJVM/test;image4s-ops-lawsJS/test"
)
addCommandAlias("fmtCheck", ";scalafmtCheckAll;scalafmtSbtCheck")
addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
