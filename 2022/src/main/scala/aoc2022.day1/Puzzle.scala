package aoc2022.day1

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(ZIO.attempt(Source.fromURL(getClass.getResource("puzzle-input.txt"))))(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))

  private val program = for {
    data <- Solution.parseInput(puzzleData)
    output1       <- Solution.solvePart1(data)
    _             <- Console.printLine(s"Result of the puzzle in part 1: " + output1)
    output2       <- Solution.solvePart2(data)
    _             <- Console.printLine(s"Result of the puzzle in part 2: " + output2)
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
