package aoc2021.day19

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day19/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))

  private val program = for {
    data    <- Solution.parseInput(puzzleData)
    space   <- Solution.createSpace(data)
    output1 <- Solution.solvePart1(space)
    _       <- Console.printLine(s"Result of the puzzle in part 1: " + output1)
    output2 <- Solution.solvePart2(space)
    _       <- Console.printLine(s"Result of the puzzle in part 2: " + output2)
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
