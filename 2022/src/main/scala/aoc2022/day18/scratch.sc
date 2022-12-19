import zio.*
import zio.stream.*
import zio.stream.ZPipeline.*
import zio.worksheet.*

// When changing file content
// use REPL
// 1. sbt clean
// 2. rebuild
// 3. change the line in code reading the file (add space or so)

val part1Pipeline = ZPipeline.mapChunks[String, Int](
  _.map(line =>
    println("p1: " + line)
    1
  )
)

val part2Pipeline = ZPipeline.mapChunks[String, Int](
  _.map(line =>
    println("p2: " + line)
    2
  )
)

val puzzle = ZIO.scoped {
  ZStream
    .fromResource("aoc2022/day18/puzzle-input-example.txt")
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

puzzle.unsafeRun
