package aoc2022.day23

import aoc2022.day23.model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): List[Boolean] = line.toCharArray.toList.map {
    case '#' => true
    case '.' => false
  }

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Area] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)
      .map(Area.fromList)

  def solvePart1(input: Area): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.iterator.drop(10).head.score)

  def solvePart2(input: Area): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(
      input.iterator
        .sliding(2)
        .map(_.toList)
        .dropWhile { case List(p, n) =>
          p.elves != n.elves
        }
        .map(_.last)
        .next()
        .round
    )

}
