import zio.*
import zio.stream.*
import zio.stream.ZPipeline.*
import zio.worksheet.*

def toNum(c: Char): Int =
  val i = c.toInt
  if i < 97 then i - 38 else i - 96

val part1Pipeline = ZPipeline.mapChunks[String, Int](
  _.map(line =>
    line.toCharArray.toList.splitAt(line.length / 2) match {
      case (left, right) => left.toSet.intersect(right.toSet).map(toNum).sum
    }
  )
)

val part2Pipeline = ZPipeline.mapChunks[String, Int](c =>
  Chunk(
    c.map(_.toCharArray.toSet).reduce(_ intersect _).map(toNum).head
  )
)

val puzzle = ZIO.scoped {
  ZStream
    .fromResource("aoc2022/day3/puzzle-input.txt")
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
