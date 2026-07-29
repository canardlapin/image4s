import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.{CrossProject, CrossType}
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

ThisBuild / organization := "io.github.canardlapin"
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / homepage := Some(url("https://github.com/canardlapin/image4s"))
ThisBuild / licenses := List(
  "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
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

lazy val ravelRevision = "f804ba51242aae3a1442b3855a20bd896ffa8b64"
lazy val ravelBuild =
  sys.props
    .get("image4s.ravel.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/ravel.git#$ravelRevision"))
lazy val ravelCoreJVM = ProjectRef(ravelBuild, "coreJVM")
lazy val ravelCoreJS  = ProjectRef(ravelBuild, "coreJS")

lazy val galeRevision = "d55fe2f97196a76ab7879e1a12f1e92403aeba06"
lazy val galeBuild =
  sys.props
    .get("image4s.gale.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/gale.git#$galeRevision"))
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS  = ProjectRef(galeBuild, "coreJS")

lazy val locus4sRevision = "4fedc7febf2728f51f6bb008ac7fe41060edc18e"
lazy val locus4sBuild =
  sys.props
    .get("image4s.locus4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(
      uri(s"https://github.com/canardlapin/locus4s.git#$locus4sRevision")
    )
lazy val locus4sCoreJVM = ProjectRef(locus4sBuild, "locus4s-coreJVM")
lazy val locus4sCoreJS  = ProjectRef(locus4sBuild, "locus4s-coreJS")

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
    .settings(name := artifact)
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
    .jvmConfigure(_.dependsOn(ravelCoreJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS))

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
    .jvmConfigure(_.dependsOn(locus4sCoreJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS))

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
      image4sLocus.js
    )
    .settings(
      name := "image4s-root",
      publish / skip := true
    )

addCommandAlias("compileAll", ";root/compile")
addCommandAlias("testAll", ";root/test")
