package aoc2021.day1

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day1/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines().map(_.toInt)))

  private val program = for {
    output1 <- Solution.increasedFromPrevious(puzzleData).runSum
    _      <- Console.printLine(s"Result of the puzzle in part 1: $output1")
    output2 <- Solution.increasedSumFromPreviousWindow(puzzleData).runSum
    _      <- Console.printLine(s"Result of the puzzle in part 2: $output2")
  } yield ()

  override def run: ZIO[Any, Any, Any] = program
}
