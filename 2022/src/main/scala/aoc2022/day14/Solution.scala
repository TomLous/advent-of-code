package aoc2022.day14

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): List[Point] = line.split(" -> ").toList.map{
    case s"$x,$y" => Point(x.toInt, y.toInt, 1)
  }

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Cave] = lineStream
    .map(parseLine)
    .runCollect
    .map(_.toList)
    .map(Cave)

  def solvePart1(cave: Cave): ZIO[Any, Throwable, Long] =
    val sandPoints = cave.removeSand.dropSandFrom(Point(500, 0, 2))
    ZIO.succeed(sandPoints.length)

  def solvePart2(cave: Cave): ZIO[Any, Throwable, Long] =
    val sandPoints = cave.removeSand.addFloor.dropSandFrom(Point(500, 0, 2))
    ZIO.succeed(sandPoints.length + 1)

}
