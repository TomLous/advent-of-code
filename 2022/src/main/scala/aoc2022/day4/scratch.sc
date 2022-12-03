import zio.*
import zio.stream.*
import zio.stream.ZPipeline.*
import zio.worksheet.*


val part1Pipeline = ZPipeline.mapChunks[String, Int](
  _.map(line =>
    0
  )
)

val part2Pipeline = ZPipeline.mapChunks[String, Int](
  _.map(line =>
    0
  )
)

val puzzle = ZIO.scoped {
  ZStream
    .fromResource("aoc2022/day4/puzzle-input-example.txt")
    .via(utf8Decode >>> splitLines)
    .broadcast(2, 5)
    .flatMap(streams =>
      for {
        s0 <- streams(0)
          .via(part1Pipeline)
          .runSum
          .fork
        s1 <- streams(1)
          .rechunk(3)
          .via(part2Pipeline)
          .runSum
          .fork
        res <- s0.join.zipPar(s1.join)
      } yield res
    )
}

puzzle.unsafeRun
