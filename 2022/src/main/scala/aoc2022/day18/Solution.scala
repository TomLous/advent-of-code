package aoc2022.day18

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Cube = line match
    case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Lava] = lineStream.map(parseLine).runCollect.map(_.toSet).map(Lava.apply)

  def solvePart1(lava: Lava): ZIO[Any, Throwable, Long] =

    ZIO.succeed(lava.exposed(false))

  def solvePart2(lava: Lava): ZIO[Any, Throwable, Long] =
    ZIO.succeed(lava.exposed(true))

}
