package aoc2022.day15

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): (Point, Point) = line match
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
      (Point(sy.toLong, sx.toLong), Point(by.toLong, bx.toLong))

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Zone] = lineStream
    .map(parseLine)
    .runCollect
    .map(_.toList)
    .map(Zone.apply)

  def solvePart1(input: Zone, row: Int): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.countVoid(row))

  def solvePart2(input: Zone, maxRangeFromO: Long): ZIO[Any, Throwable, Long] =
    val point = input.findNondetecteableBeacon(maxRangeFromO)
    ZIO.succeed(point.tuningFrequency)

}
