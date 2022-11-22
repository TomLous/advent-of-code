package aoc2021.day7

import aoc2021.day7.model.*
import zio.*
import zio.stream.ZStream

object Solution {




  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Int]] = lineStream
    .map(_.split(",").map(_.toInt)).runCollect.map(_.toList.flatten)


  def solvePart1(input: List[Int]): ZIO[Any, Throwable, Long] = {
    val cp = CrabPositions(input)
    ZIO.succeed(cp.calculateOptimalPosition(true).fuel)
  }

  def solvePart2(input: List[Int]): ZIO[Any, Throwable, Long] = {
    val cp = CrabPositions(input)
    ZIO.succeed(cp.calculateOptimalPosition(false).fuel)
  }

}
