import zio.*
import zio.stream.*
import zio.stream.ZPipeline.*
import zio.worksheet.*
import scala.io.Source

//val resourcePath = "/Users/tomlous/Development/Scala/advent-of-code/2022/src/main/resources/aoc2022/day5/"
//val in = Source.fromFile(resourcePath + "puzzle-input-example2.txt?" + math.random()).getLines().mkString("\n")


object app {
  val part1Pipeline = ZPipeline.mapChunks[String, Int](
    _.map(line =>
      println("p1: " + line)
      2
    )
  )

  val part2Pipeline = ZPipeline.mapChunks[String, Int](
    _.map(line =>
//      println("p2: " + line)
      1
    )
  )



  //resources/aoc2022/day5/puzzle-input-example.txt
  val puzzle = ZIO.scoped {
    ZStream
      .fromResource("aoc2022/day5/puzzle-input-example.txt")
      .via(utf8Decode >>> splitLines)
      .broadcast(2, 5)
      .flatMap(streams =>
        for {
          s0 <- streams(0)
            .via(part1Pipeline)
            .runSum
            .fork
          s1 <- streams(1)
            .via(part2Pipeline)
            .runSum
            .fork
          res <- s0.join.zipPar(s1.join)
        } yield res
      )
  }
}

app.puzzle.unsafeRun

