lazy val commonSettings = Seq(
  version := "0.0.6",
  organization := "net.katsstuff",
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  }
)

lazy val commonScala2Settings = commonSettings ++ Seq(
  scalaVersion := "2.13.3",
  moduleName := {
    val old = moduleName.value
    if (old == "perspective") "perspectivescala2"
    else s"perspectivescala2-$old"
  },
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.2").cross(CrossVersion.full)),
  scalacOptions += "-explaintypes"
)

lazy val commonDottySettings = commonSettings ++ Seq(
  scalaVersion := "3.0.2-RC2",
  moduleName := {
    val old = moduleName.value
    if (old == "perspective") old
    else s"perspective-$old"
  },
  scalacOptions += "-Ykind-projector"
  //libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % scalaVersion.value
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  Test / publishArtifact := false,
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
  autoAPIMappings := true
)

lazy val noPublishSettings = Seq(publish := {}, publishLocal := {}, publishArtifact := false)

lazy val scala2Perspective = project
  .in(file("scala2/perspective"))
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "perspective",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.typelevel" %% "cats-core"  % "2.3.1",
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

lazy val circeVersion = "0.14.1"

lazy val scala2PerspectiveDerivation = project
  .in(file("scala2/derivation"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "derivation",
    libraryDependencies += "com.chuusai"   %% "shapeless"     % "2.3.3",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion % Test),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
  )

lazy val scala2PerspectiveExamples = project
  .in(file("scala2/examples"))
  .dependsOn(scala2PerspectiveDerivation, scala2PerspectiveMacros)
  .settings(
    commonScala2Settings,
    noPublishSettings,
    name := "examples",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
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
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
  )

lazy val dottyPerspectiveDerivation = project
  .in(file("dotty/derivation"))
  .dependsOn(dottyPerspective)
  .settings(
    commonDottySettings,
    publishSettings,
    name := "derivation"
  )

lazy val dottyPerspectiveExamples = project
  .in(file("dotty/examples"))
  .dependsOn(dottyPerspectiveDerivation)
  .settings(
    commonDottySettings,
    noPublishSettings,
    name := "examples",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
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
  .settings(
    commonSettings,
    noPublishSettings
  )


lazy val perspectiveDotty =
  project
    .in(file("dotty"))
    .aggregate(dottyPerspective, dottyPerspectiveDerivation, dottyPerspectiveExamples)
    .settings(
      commonSettings,
      noPublishSettings
    )

lazy val perspectiveRoot =
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
    .settings(
      commonSettings,
      noPublishSettings
    )
