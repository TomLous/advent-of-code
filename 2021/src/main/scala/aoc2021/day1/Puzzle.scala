package aoc2021.day1

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines().map(_.toInt)))

  private val program = for {
    output <- Solution.increasedFromPrevious(puzzleData).runSum
    _      <- Console.printLine(s"Result of the puzzle: $output")
  } yield ()

  override def run: ZIO[Any, Any, Any] = program
}
