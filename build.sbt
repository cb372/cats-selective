val catsVersion = "2.1.0"

val commonSettings = Seq(
  organization := "org.typelevel",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-deprecation"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "com.github.mpilquist" %% "simulacrum" % "0.19.0"
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
      "org.typelevel" %% "cats-testkit-scalatest" % "1.0.0-RC1" % Test
  )
  .dependsOn(core, laws)
