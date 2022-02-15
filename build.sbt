ThisBuild / version := "1.0.0-preview-01"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "com.dadhi"

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.7"

lazy val speedy = (project in file("."))
  .settings(
    name := "Speedy",
    libraryDependencies += scalaTest % Test,
    resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases",
    libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.21"
  )
