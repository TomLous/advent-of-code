package aoc2021.day5

import aoc2021.day4.model.*
import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day5/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))

  private val program = for {
    lineSegments <- Solution.parseInput(puzzleData)
    output1       <- Solution.solvePart1(lineSegments)
    _           <- Console.printLine(s"Result of the puzzle in part 1: $output1")
    output2       <- Solution.solvePart2(lineSegments)
    _           <- Console.printLine(s"Result of the puzzle in part 2: $output2")
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
