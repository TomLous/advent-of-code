package aoc2020.day1

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line.toInt

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    val res = input.combinations(2).find(_.sum == 2020).map(_.product).get
    ZIO.succeed(res)

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    val res = input.combinations(3).find(_.sum == 2020).map(_.product).get
    ZIO.succeed(res)

}
