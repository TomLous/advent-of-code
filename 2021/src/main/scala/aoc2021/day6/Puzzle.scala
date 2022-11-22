package aoc2021.day6

import aoc2021.day4.model.*
import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day6/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))

  private val program = for {
    lanternFish <- Solution.parseInput(puzzleData)
    output1     <- Solution.solvePart1(lanternFish, 80)
    _           <- Console.printLine(s"Result of the puzzle in part 1: $output1")
    output2     <- Solution.solvePart1(lanternFish, 256)
    _           <- Console.printLine(s"Result of the puzzle in part 2: $output2")
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
