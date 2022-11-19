package aoc2021.day2

import model.DiveInstruction
import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day2/puzzle.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()).mapZIO(s => ZIO.fromEither(DiveInstruction.fromString(s))
  ))

  private val program = for{
    output1 <- Solution.followDiveInstructions(puzzleData)
    _       <- Console.printLine(s"Result of the puzzle in part 1: ${output1.mult}")
  }yield ()


  override def run: ZIO[Any, Any, Any] = program

}
