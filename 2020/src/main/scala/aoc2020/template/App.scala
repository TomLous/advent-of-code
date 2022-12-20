package aoc2020.template

import zio.*
import zio.stream.*
import zio.stream.ZPipeline.*
import zio.worksheet.*

import scala.io.Source

// Use this for the easy apps
case object App extends ZIOAppDefault {

  private val part1Pipeline = ZPipeline.mapChunks[String, Int](
    _.map(line =>
      println("p1: " + line)
      2
    )
  )

  private val part2Pipeline = ZPipeline.mapChunks[String, Int](
    _.map(line =>
//      println("p2: " + line)
      1
    )
  )

  private val puzzle = ZIO.scoped {
    ZStream
      .fromResource("aoc2022/template/puzzle-input-example.txt")
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
          _   <- Console.printLine(s"\np1: ${res._1} \np2: ${res._2}")
        } yield res
      )
  }

  override def run: ZIO[Any, Any, Any] = puzzle

}
