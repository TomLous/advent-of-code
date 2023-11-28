package aoc2022.day10

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input =  line match
    case "noop" => Noop
    case s"addx $x" => AddX(x.toInt)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, CRT] =
    lineStream
    .map(parseLine) // TODO make this streaming with zipping the pos with the Input etc etc
    .runCollect
    .map(_.toList)
    .map(CRT.apply)

  def solvePart1(input: CRT): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.signalStrength)


  def solvePart2(input: CRT): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.printCRT)

}
