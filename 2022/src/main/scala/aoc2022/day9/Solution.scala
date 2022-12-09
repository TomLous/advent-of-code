package aoc2022.day9

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line match
    case s"$dir $step" => Input(Direction(dir.head), step.toInt)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, RopeBridge] = lineStream.map(parseLine).runCollect.map(_.toList).map(RopeBridge)

  def solvePart1(input: RopeBridge): ZIO[Any, Throwable, Long] =

    val (_, tailPos) = input.run(1)
    ZIO.succeed(tailPos(0).distinct.size)

  def solvePart2(input: RopeBridge): ZIO[Any, Throwable, Long] =
    val (_, tailPos) = input.run(9)
    ZIO.succeed(tailPos(8).distinct.size)


}
