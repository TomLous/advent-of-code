package aoc2021.day5

import aoc2021.day5.model.*
import zio.*
import zio.stream.ZStream

object Solution {


  private def parseLine(str: String): LineSegment = str.split("->").toList match {
    case List(pointAstr, pointBstr) => LineSegment(parsePoint(pointAstr), parsePoint(pointBstr))
    case _ => throw new RuntimeException(s"Invalid input: $str")
  }

  private def parsePoint(str: String): Point = str.split(",").toList match {
    case List(xStr, yStr) => Point(xStr.trim.toInt, yStr.trim.toInt)
    case _ => throw new RuntimeException(s"Invalid input: $str")
  }

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[LineSegment]] = lineStream.map(parseLine).runCollect.map(_.toList)


  def solvePart1(input: List[LineSegment]): ZIO[Any, Throwable, Int] =
    ZIO.succeed(OceanFloor(input, onlyStraightLines = true).pointMap.count(_._2 > 1))

  def solvePart2(input: List[LineSegment]): ZIO[Any, Throwable, Int] =
    ZIO.succeed(OceanFloor(input, onlyStraightLines = false).pointMap.count(_._2 > 1))


}
