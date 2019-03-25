val catsVersion = "1.6.0"

val commonSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.12.8",
  // TODO crossScalaVersions
  scalacOptions ++= Seq(
    "-language:higherKinds"
  ),
  addCompilerPlugin(("org.spire-math" % "kind-projector" % "0.9.9").cross(CrossVersion.binary)),
  addCompilerPlugin(("org.scalamacros" % "paradise" % "2.1.0").cross(CrossVersion.full)),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "com.github.mpilquist" %% "simulacrum" % "0.15.0"
  )
)

val core = project
  .in(file("core"))
  .settings(commonSettings)

val laws = project
  .in(file("laws"))
  .settings(commonSettings)
  .settings(
    libraryDependencies +=
      "org.typelevel" %% "cats-laws" % catsVersion
  )
  .dependsOn(core)

val tests = project
  .in(file("tests"))
  .settings(commonSettings)
  .settings(
    libraryDependencies +=
      "org.typelevel" %% "cats-testkit" % catsVersion % Test
  )
  .dependsOn(core, laws)
