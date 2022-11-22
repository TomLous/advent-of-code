package aoc2021.day4

import aoc2021.day4.model.*
import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day4/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))

  private val program = for {
    parsed      <- Solution.parseInput(puzzleData)
    resultPart1 <- Solution.solvePart1(parsed.nums, parsed.boards)
    _           <- Console.printLine(s"Result of the puzzle in part 1: ${resultPart1.solution}")
    resultPart2 <- Solution.solvePart2(parsed.nums, parsed.boards)
    _           <- Console.printLine(s"Result of the puzzle in part 2: ${resultPart2.solution}")
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
