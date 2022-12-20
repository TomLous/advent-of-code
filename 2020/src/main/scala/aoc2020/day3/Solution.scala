package aoc2020.day3

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line.toCharArray.map {
    case '.' => 0
    case '#' => 1
  }.toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Toboggan] = lineStream.map(parseLine).runCollect.map(_.toList).map(Toboggan.apply)

  def solvePart1(input: Toboggan): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.countTrees(3, 1))

  def solvePart2(input: Toboggan): ZIO[Any, Throwable, BigInt] =
    val routes = List(
      input.countTrees(1, 1),
      input.countTrees(3, 1),
      input.countTrees(5, 1),
      input.countTrees(7, 1),
      input.countTrees(1, 2)
    )
    ZIO.succeed(routes.product)

}
