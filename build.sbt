ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "dadhi"

Test / parallelExecution := false

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.7"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4"
val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.21"
val scalaMeterCore = "com.storm-enroute" %% "scalameter-core" % "0.21"

val projScalacOptions = List(
  "-unchecked",
  "-deprecation",
  "-language:_",
  "-feature",
  "-Xlint:_",
  "-encoding",
  "UTF-8"
)

lazy val speedy = (project in file("."))
  .settings(
    name := "Speedy",
    version := "1.0.0-preview-01",
    scalacOptions := projScalacOptions,
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test,
    resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases",
    libraryDependencies ++= Seq(scalaMeter, scalaMeterCore),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false
  )
