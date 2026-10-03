ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.9.0"

lazy val root = (project in file("."))
  .settings(
    name := "markdown-analyzer",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.9"
    ),
    assembly / assemblyJarName := "markdown-analyzer.jar",
    assembly / mainClass := Some("markdown.Main")
  )