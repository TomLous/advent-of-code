package aoc2021.day3

import aoc2021.day3.model.*
import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day3/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines().map(_.toList.map(_.asDigit))))

  private val program = for {
    tree    <- Solution.createBinaryTree(puzzleData)
    output1 <- Solution.calculatePowerConsumption(puzzleData)
    _       <- Console.printLine(s"Result of the puzzle in part 1: ${output1.mult}")
    output2 <- Solution.calculateLifeSupport(tree)
    _       <- Console.printLine(s"Result of the puzzle in part 2: ${output2.mult}")
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
