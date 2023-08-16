ThisBuild / scalaVersion := "2.13.11"
ThisBuild / organization := "dadhi"

lazy val docs = (project in file("."))
  .enablePlugins(MdocPlugin)
  .settings(
    name := "Docs",
    version := "1.0",
    Compile / unmanagedSourceDirectories += baseDirectory.value / ".." / "src" / "main" / "scala"
  )
