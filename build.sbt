lazy val commonSettings = Seq(
  version := "0.0.2",
  organization := "net.katsstuff"
)

lazy val commonScala2Settings = commonSettings ++ Seq(
  scalaVersion := "2.13.1",
  moduleName := s"perspective-${moduleName.value}",
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)),
  scalacOptions += "-explaintypes"
)

lazy val commonDottySettings = commonSettings ++ Seq(
  scalaVersion := "0.20.0-RC1",
  moduleName := s"perspectivedotty-${moduleName.value}",
  scalacOptions += "-Ykind-projector",
  libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % scalaVersion.value
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/Katrix/perspective"),
      "scm:git:github.com/Katrix/perspective",
      Some("scm:git:github.com/Katrix/perspective")
    )
  ),
  homepage := Some(url("https://github.com/Katrix/perspective")),
  developers := List(Developer("Katrix", "Kat", "katrix97@hotmail.com", url("http://katsstuff.net/"))),
  pomIncludeRepository := (_ => false),
  autoAPIMappings := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  }
)

lazy val scala2Perspective = project
  .in(file("scala2/perspective"))
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "perspective",
	moduleName := "perspective",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.typelevel" %% "cats-core"  % "2.0.0",
    libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"
  )

lazy val scala2PerspectiveMacro = project
  .in(file("scala2/perspectiveMacro"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "perspective-macro",
    libraryDependencies += "org.scala-lang" % "scala-reflect"        % scalaVersion.value % Provided,
    libraryDependencies += "org.typelevel"  %% "cats-tagless-macros" % "0.10"
  )

lazy val dottyPerspective = project
  .in(file("dotty/perspective"))
  .settings(
    commonDottySettings,
    publishSettings,
    name := "category",
	moduleName := "perspectivedotty"
  )

lazy val perspectiveScala2 = project.in(file("scala2")).aggregate(scala2Perspective, scala2PerspectiveMacro)
lazy val perspectiveDotty  = project.in(file("dotty")).aggregate(dottyPerspective)

lazy val PerspectiveRoot =
  project.in(file(".")).aggregate(scala2Perspective, scala2PerspectiveMacro, dottyPerspective)
