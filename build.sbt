lazy val commonSettings = Seq(
  version := "0.0.3",
  organization := "net.katsstuff"
)

lazy val commonScala2Settings = commonSettings ++ Seq(
  scalaVersion := "2.13.3",
  moduleName := s"perspective-${moduleName.value}",
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.2").cross(CrossVersion.full)),
  scalacOptions += "-explaintypes"
)

lazy val commonDottySettings = commonSettings ++ Seq(
  scalaVersion := "0.27.0-RC1",
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
  developers := List(Developer("Katrix", "Kathryn", "katrix97@hotmail.com", url("http://katsstuff.net/"))),
  pomIncludeRepository := (_ => false),
  autoAPIMappings := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  }
)

lazy val noPublishSettings = Seq(publish := {}, publishLocal := {}, publishArtifact := false)

lazy val scala2Perspective = project
  .in(file("scala2/perspective"))
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "perspective",
    moduleName := "perspective",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.typelevel" %% "cats-core"  % "2.3.0",
    libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.1"
  )

lazy val scala2PerspectiveParameterized = project
  .in(file("scala2/parameterized"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "parameterized",
    scalacOptions += "-Ymacro-annotations"
  )

lazy val scala2PerspectiveDerivation = project
  .in(file("scala2/derivation"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "derivation",
    libraryDependencies += "com.chuusai"   %% "shapeless"     % "2.3.3",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
  )

lazy val scala2PerspectiveExamples = project
  .in(file("scala2/examples"))
  .dependsOn(scala2PerspectiveDerivation, scala2PerspectiveMacros)
  .settings(
    commonScala2Settings,
    noPublishSettings,
    name := "examples",
    scalacOptions += "-Ymacro-annotations"
  )

lazy val scala2PerspectiveMacros = project
  .in(file("scala2/macros"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "macro",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.scala-lang" % "scala-reflect"       % scalaVersion.value % Provided,
    libraryDependencies += "org.typelevel" %% "cats-tagless-macros" % "0.12"
  )

lazy val dottyPerspective = project
  .in(file("dotty/perspective"))
  .settings(
    commonDottySettings,
    publishSettings,
    name := "perspective",
    moduleName := "perspectivedotty",
    libraryDependencies += ("org.typelevel" %% "cats-core" % "2.3.0").withDottyCompat(scalaVersion.value)
  )

lazy val dottyPerspectiveDerivation = project
  .in(file("dotty/derivation"))
  .dependsOn(dottyPerspective)
  .settings(
    commonDottySettings,
    publishSettings,
    name := "derivation",
    moduleName := "derivationdotty"
  )

lazy val dottyPerspectiveExamples = project
  .in(file("dotty/examples"))
  .dependsOn(dottyPerspectiveDerivation)
  .settings(
    commonDottySettings,
    noPublishSettings,
    name := "examples"
    //scalacOptions ++= Seq("-Xprint:typer")
  )

lazy val perspectiveScala2 = project
  .in(file("scala2"))
  .aggregate(
    scala2Perspective,
    scala2PerspectiveParameterized,
    scala2PerspectiveDerivation,
    scala2PerspectiveExamples,
    scala2PerspectiveMacros
  )
lazy val perspectiveDotty =
  project.in(file("dotty")).aggregate(dottyPerspective, dottyPerspectiveDerivation, dottyPerspectiveExamples)

lazy val PerspectiveRoot =
  project
    .in(file("."))
    .aggregate(
      scala2Perspective,
      scala2PerspectiveParameterized,
      scala2PerspectiveDerivation,
      scala2PerspectiveExamples,
      scala2PerspectiveMacros,
      dottyPerspective,
      dottyPerspectiveDerivation,
      dottyPerspectiveExamples
    )
