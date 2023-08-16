ThisBuild / scalaVersion := "2.13.11"
ThisBuild / organization := "dadhi"

// Just for giggles
Compile / sourceGenerators += Def.task {
    val file = (Compile / sourceManaged).value / "demo" / "Test.scala"
    IO.write(file, Generator.generate)
    Seq(file)
}.taskValue

val projScalacOptions = List(
  "-unchecked",
  "-deprecation",
  "-language:_",
  "-feature",
  "-Xlint:_",
  "-encoding",
  "UTF-8"
)

lazy val benchmarks = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "Benchmarks",
    version := "1.0",
    scalacOptions := projScalacOptions,
    Compile / unmanagedSourceDirectories += baseDirectory.value / ".." / "src" / "main" / "scala"
  )
