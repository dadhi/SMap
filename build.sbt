ThisBuild / version := "1.0.0-preview-01"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "com.dadhi"

Test / parallelExecution := false

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.7"
val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.21"
val scalaMeterCore = "com.storm-enroute" %% "scalameter-core" % "0.21"

lazy val speedy = (project in file("."))
  .settings(
    name := "Speedy",
    scalacOptions := List("-feature", "-Xlint", "-deprecation", "-unchecked"),
    libraryDependencies += scalaTest % Test,
    resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases",
    libraryDependencies ++= Seq(scalaMeter, scalaMeterCore),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false
  )
