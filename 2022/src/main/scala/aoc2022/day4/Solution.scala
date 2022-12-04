package aoc2022.day4

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line match
    case s"$a1-$a2,$b1-$b2" => ((a1.toInt to a2.toInt).toSet, (b1.toInt to b2.toInt).toSet)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.count { case (a, b) => a.subsetOf(b) || b.subsetOf(a) }.toLong)

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.count{ case (a, b) => (a intersect b).nonEmpty }.toLong)

}
