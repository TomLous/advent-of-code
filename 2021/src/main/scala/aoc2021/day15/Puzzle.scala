package aoc2021.day15

import zio.*
import zio.stream.*

import scala.io.Source

object Puzzle extends ZIOAppDefault {

  private val puzzleData = ZStream
    .acquireReleaseWith(
      ZIO.attempt(Source.fromResource("day15/puzzle-input.txt"))
    )(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))

  private def logging[T](logLine: String, appendOutput: Boolean = false)(tuple: (Duration, T)): ZIO[Any, Throwable, T] = tuple match
    case (duration, output) => Console.printLine(logLine + (if appendOutput then output.toString else "") + s" [${duration.toMillis}ms]").as(output)

  private val program = for {
    data         <- Solution.parseInput(puzzleData).timed.flatMap(logging("Parsed data"))
    _            <- Solution.solvePart1(data).timed.flatMap(logging("Result of the puzzle in part 1: ", appendOutput = true))
    dataExpanded <- Solution.parseInput(puzzleData, true).timed.flatMap(logging("Parsed data"))
    _            <- Solution.solvePart2(dataExpanded).timed.flatMap(logging("Result of the puzzle in part 2: ", appendOutput = true))
  } yield ()

  override def run: ZIO[Any, Any, Any] = program

}
