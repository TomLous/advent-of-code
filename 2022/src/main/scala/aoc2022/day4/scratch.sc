import zio.*
import zio.stream.*
import zio.stream.ZPipeline.*
import zio.worksheet.*

val toSet = ZPipeline.mapChunks[String, (Set[Int], Set[Int])](_.map{
  case s"$a1-$a2,$b1-$b2" => ((a1.toInt to a2.toInt).toSet, (b1.toInt to b2.toInt).toSet)
})


val puzzle = ZIO.scoped {
  ZStream
    .fromResource("aoc2022/day4/puzzle-input.txt")
    .via(utf8Decode >>> splitLines >>> toSet)
    .broadcast(2, 5)
    .flatMap(streams =>
      for {
        s0 <- streams(0)
          .filter{case (a, b) => a.diff(b).isEmpty || b.diff(a).isEmpty}
          .runCount
          .fork
        s1 <- streams(1)
          .filter{case (a, b) => a.intersect(b).nonEmpty || b.intersect(a).nonEmpty}
          .runCount
          .fork
        res <- s0.join.zipPar(s1.join)
      } yield res
    )
}

puzzle.unsafeRun
