package aoc2022.day25

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends AdventOfCodeApp {

  override val program: ZIO[Any, Throwable, Unit] = for {
    data      <- Solution.parseInput(puzzleData).timed.flatMap(aocLogging("Parsed data"))
    _         <- Solution.solvePart1(data).timed.flatMap(aocLogging("Result of the puzzle in part 1: ", appendOutput = true))
  } yield ()


}
