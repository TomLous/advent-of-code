package aoc2022.day15

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(ZIO.attempt(Source.fromURL(getClass.getResource("puzzle-input.txt"))))(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))
  private def logging[T](logLine:String, appendOutput: Boolean=false)(tuple: (Duration, T)):ZIO[Any, Throwable, T] = tuple match
    case (duration, output) => Console.printLine(logLine + (if appendOutput then output.toString else "") + s" [${duration.toMillis}ms]").as(output)

  private val program = for {
    data      <- Solution.parseInput(puzzleData).timed.flatMap(logging("Parsed data"))
    _         <- Solution.solvePart1(data, 2000000).timed.flatMap(logging("Result of the puzzle in part 1: ", appendOutput = true))
    _         <- Solution.solvePart2(data, 4000000).timed.flatMap(logging("Result of the puzzle in part 2: ", appendOutput = true))
  } yield ()

  override def run: ZIO[Any, Any, Any] = program.timed.flatMap(logging("Completed year 2022 day 15")).exitCode

}
