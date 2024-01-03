import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

inThisBuild(
  Seq(
    homepage      := Some(url("https://github.com/Katrix/perspective")),
    organization  := "net.katsstuff",
    licenses      := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
    developers    := List(Developer("Katrix", "Kathryn Frid", "katrix97@hotmail.com", url("http://katsstuff.net/"))),
    versionScheme := Some("early-semver")
  )
)

lazy val commonScala2Settings = Seq(
  scalaVersion := "2.13.12",
  moduleName := {
    val old = moduleName.value
    if (old == "perspective") "perspectivescala2"
    else s"perspectivescala2-$old"
  },
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.2").cross(CrossVersion.full)),
  scalacOptions += "-explaintypes"
)

lazy val commonDottySettings = Seq(
  scalaVersion := "3.3.1",
  moduleName := {
    val old = moduleName.value
    if (old == "perspective") old
    else s"perspective-$old"
  },
  scalacOptions += "-Ykind-projector",
  scalacOptions += "-feature",
  libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.13" % Test,
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % Test
)

lazy val publishSettings = Seq(
  Test / publishArtifact := false,
  autoAPIMappings        := true
)

lazy val noPublishSettings = Seq(publish := {}, publishLocal := {}, publishArtifact := false, publish / skip := true)

lazy val scala2Perspective = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("scala2/perspective"))
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "perspective",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.typelevel" %%% "cats-core"  % "2.8.0",
    libraryDependencies += "org.typelevel" %%% "simulacrum" % "1.0.1"
  )

lazy val scala2PerspectiveJVM = scala2Perspective.jvm
lazy val scala2PerspectiveJS  = scala2Perspective.js

lazy val circeVersion = "0.14.2"

lazy val scala2PerspectiveDerivation = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("scala2/derivation"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name                                   := "derivation",
    libraryDependencies += "com.chuusai"  %%% "shapeless"     % "2.3.9",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion % Test),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % Test
  )

lazy val scala2PerspectiveDerivationJVM = scala2PerspectiveDerivation.jvm
lazy val scala2PerspectiveDerivationJS  = scala2PerspectiveDerivation.js

lazy val scala2PerspectiveMacros = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("scala2/macros"))
  .dependsOn(scala2Perspective)
  .settings(
    commonScala2Settings,
    publishSettings,
    name := "macro",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.scala-lang"  % "scala-reflect"       % scalaVersion.value % Provided,
    libraryDependencies += "org.typelevel" %%% "cats-tagless-macros" % "0.12"
  )

lazy val scala2PerspectiveMacrosJVM = scala2PerspectiveMacros.jvm
lazy val scala2PerspectiveMacrosJS  = scala2PerspectiveMacros.js

lazy val scala2PerspectiveExamples = project
  .in(file("scala2/examples"))
  .dependsOn(scala2PerspectiveDerivationJVM, scala2PerspectiveMacrosJVM)
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

lazy val dottyPerspective = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("dotty/perspective"))
  .settings(
    commonDottySettings,
    publishSettings,
    name                                    := "perspective",
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.8.0"
  )

lazy val dottyPerspectiveJVM = dottyPerspective.jvm
lazy val dottyPerspectiveJS  = dottyPerspective.js

lazy val dottyPerspectiveDerivation = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("dotty/derivation"))
  .dependsOn(dottyPerspective)
  .settings(
    commonDottySettings,
    publishSettings,
    name := "derivation",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion % Test)
  )

lazy val dottyPerspectiveDerivationJVM = dottyPerspectiveDerivation.jvm
lazy val dottyPerspectiveDerivationJS  = dottyPerspectiveDerivation.js

lazy val dottyPerspectiveExamples = project
  .in(file("dotty/examples"))
  .dependsOn(dottyPerspectiveDerivationJVM)
  .settings(
    commonDottySettings,
    noPublishSettings,
    name := "examples",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
    // scalacOptions ++= Seq("-Xprint:typer")
  )

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docs = project
  .enablePlugins(MicrositesPlugin, ScalaUnidocPlugin, GhpagesPlugin)
  .settings(
    commonDottySettings,
    micrositeName                          := "perspective",
    micrositeAuthor                        := "Katrix",
    micrositeDescription                   := "Higher kinded data in Scala",
    micrositeDocumentationUrl              := "/api/perspective",
    micrositeDocumentationLabelDescription := "ScalaDoc",
    micrositeHomepage                      := "https://perspective.katsstuff.net",
    micrositeGithubOwner                   := "Katrix",
    micrositeGithubRepo                    := "perspective",
    micrositeGitterChannel                 := false,
    micrositeShareOnSocial                 := false,
    micrositeTheme                         := "pattern",
    ghpagesCleanSite / excludeFilter       := "CNAME",
    micrositePushSiteWith                  := GitHub4s,
    micrositeGithubToken                   := sys.env.get("GITHUB_TOKEN"),
    autoAPIMappings := true,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      dottyPerspectiveJVM,
      dottyPerspectiveDerivationJVM,
    ),
    docsMappingsAPIDir := "api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, docsMappingsAPIDir),
    //mdoc / fork := true,
    mdocIn := sourceDirectory.value / "main" / "mdoc",
    //ScalaUnidoc / unidoc / fork := true,
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-doc-source-url",
      "https://github.com/Katrix/perspective/tree/master€{FILE_PATH}.scala",
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath
    )
  )

lazy val perspectiveScala2 = project
  .in(file("scala2"))
  .aggregate(
    scala2PerspectiveJVM,
    scala2PerspectiveJS,
    scala2PerspectiveDerivationJVM,
    scala2PerspectiveDerivationJS,
    scala2PerspectiveMacrosJVM,
    scala2PerspectiveMacrosJS,
    scala2PerspectiveExamples
  )
  .settings(
    noPublishSettings
  )

lazy val perspectiveDotty =
  project
    .in(file("dotty"))
    .aggregate(
      dottyPerspectiveJVM,
      dottyPerspectiveJS,
      dottyPerspectiveDerivationJVM,
      dottyPerspectiveDerivationJS,
      dottyPerspectiveExamples
    )
    .settings(
      noPublishSettings
    )

lazy val perspectiveRoot =
  project
    .in(file("."))
    .aggregate(
      scala2PerspectiveJVM,
      scala2PerspectiveJS,
      scala2PerspectiveDerivationJVM,
      scala2PerspectiveDerivationJS,
      scala2PerspectiveMacrosJVM,
      scala2PerspectiveMacrosJS,
      scala2PerspectiveExamples,
      dottyPerspectiveJVM,
      dottyPerspectiveJS,
      dottyPerspectiveDerivationJVM,
      dottyPerspectiveDerivationJS,
      dottyPerspectiveExamples
    )
    .settings(
      noPublishSettings
    )
