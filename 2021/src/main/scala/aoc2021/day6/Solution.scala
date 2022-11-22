package aoc2021.day6

import aoc2021.day6.model.*
import zio.*
import zio.stream.ZStream

object Solution {




  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Int]] = lineStream
    .map(_.split(",").map(_.toInt)).runCollect.map(_.toList.flatten)

  def solvePart1(input: List[Int], iterations:Int): ZIO[Any, Throwable, Long] = {
    ZIO.succeed(School(input).iterate(iterations).size)
  }
  


}
