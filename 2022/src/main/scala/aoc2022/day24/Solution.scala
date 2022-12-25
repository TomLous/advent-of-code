package aoc2022.day24

import aoc2022.day24.model.*
import zio.*
import zio.stream.*

object Solution {


  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Basin] =
    lineStream
      .runCollect
      .map(_.toList)
      .map(Basin.fromLines)

  def solvePart1(input: Basin): ZIO[Any, Throwable, BigInt] =
    val p = input.shortestPath(input.startPoint, Seq(input.endPoint)).getOrElse(BigInt(0))
    ZIO.succeed(p)

  def solvePart2(input: Basin): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.shortestPath(input.startPoint, Seq(input.endPoint, input.startPoint, input.endPoint)).getOrElse(BigInt(0)))

}
