ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code"
  )
  .aggregate(aoc2020, aoc2021, aoc2022, aoc2023)

lazy val zio2Libs = Seq(
  "dev.zio"     %% "zio"               % "2.0.19",
  "dev.zio"     %% "zio-streams"       % "2.0.19",
  "dev.zio"     %% "zio-test"          % "2.0.19" % Test,
  "dev.zio"     %% "zio-test-sbt"      % "2.0.19" % Test,
  "dev.zio"     %% "zio-test-magnolia" % "2.0.19" % Test,
  "dev.zio"     %% "zio-optics"        % "0.2.1",
  "dev.zio"     %% "zio-json"          % "0.6.2",
  "fr.janalyse" %% "zio-worksheet"     % "2.0.19.0"
)

lazy val linAlgLibs = Seq(
  "org.scalanlp" %% "breeze" % "2.1.0"
)


lazy val oldGraphLibs = Seq(
  "org.scala-graph" % "graph-core_2.13" % "1.13.5"
)

lazy val graphLibs = Seq(
  "org.scala-graph" % "graph-core_2.13" % "2.0.0"
)

lazy val parallelLibs = Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

lazy val defaults = Seq(
  scalaVersion := "3.3.1",
  libraryDependencies ++= zio2Libs ++ linAlgLibs  ++ parallelLibs,
  Test / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

lazy val aoc2019 = (project in file("2019"))
  .settings(defaults)
  .settings(
    libraryDependencies ++= graphLibs,
    moduleName := "2019",
  )
  .dependsOn(util)

lazy val aoc2020 = (project in file("2020"))
  .settings(defaults)
  .settings(
    libraryDependencies ++= graphLibs,
    moduleName   := "2020",
  ).dependsOn(util)

lazy val aoc2021 = (project in file("2021"))
  .settings(defaults)
  .settings(
    libraryDependencies ++= oldGraphLibs,
    moduleName := "2021"
  )
  .dependsOn(util)

lazy val aoc2022 = (project in file("2022"))
  .settings(defaults)
  .settings(
    libraryDependencies ++= oldGraphLibs,
    moduleName := "2022"
  )
  .dependsOn(util)

lazy val aoc2023 = (project in file("2023"))
  .settings(defaults)
  .settings(
    libraryDependencies ++= graphLibs,
    moduleName := "2023",
  )
  .dependsOn(util)





lazy val util = (project in file("util"))
  .settings(defaults)
  .settings(
    moduleName := "util"
  )


