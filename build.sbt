ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code"
  )
  .aggregate(aoc2020, aoc2021, aoc2022)

lazy val zio2Libs = Seq(
  "dev.zio"     %% "zio"               % "2.0.4",
  "dev.zio"     %% "zio-streams"       % "2.0.4",
  "dev.zio"     %% "zio-test"          % "2.0.4" % Test,
  "dev.zio"     %% "zio-test-sbt"      % "2.0.4" % Test,
  "dev.zio"     %% "zio-test-magnolia" % "2.0.4" % Test,
  "dev.zio"     %% "zio-optics"        % "0.2.0",
  "dev.zio"     %% "zio-json"          % "0.4.2",
  "fr.janalyse" %% "zio-worksheet"     % "2.0.4.0"
)

lazy val linAlgLibs = Seq(
  "org.scalanlp" %% "breeze" % "2.1.0"
)

lazy val graphLibs = Seq(
  "org.scala-graph" % "graph-core_2.13" % "1.13.5"
)

lazy val parallelLibs = Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

lazy val aoc2020 = (project in file("2020"))
  .settings(
    scalaVersion := "3.2.1",
    moduleName   := "2020",
    libraryDependencies ++= zio2Libs ++ linAlgLibs ++ graphLibs ++ parallelLibs,
    Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  ).dependsOn(util)

lazy val aoc2021 = (project in file("2021"))
  .settings(
    scalaVersion := "3.2.1",
    moduleName   := "2021",
    libraryDependencies ++= zio2Libs ++ linAlgLibs ++ graphLibs ++ parallelLibs,
    Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  ).dependsOn(util)

lazy val aoc2022 = (project in file("2022"))
  .settings(
    scalaVersion := "3.2.1",
    moduleName   := "2022",
    libraryDependencies ++= zio2Libs ++ linAlgLibs ++ graphLibs ++ parallelLibs,
    Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  ).dependsOn(util)

lazy val util = (project in file("util"))
  .settings(
    scalaVersion := "3.2.1",
    moduleName   := "2022",
    libraryDependencies ++= zio2Libs ++ linAlgLibs ++ graphLibs ++ parallelLibs,
    Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
