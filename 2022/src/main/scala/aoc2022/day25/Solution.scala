package aoc2022.day25

import aoc2022.day25.model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String):SNAFU = SNAFU(line)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[SNAFU]] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)


  def solvePart1(input: List[SNAFU]): ZIO[Any, Throwable, String] =
    ZIO.succeed(SNAFU.fromDec(input.map(_.dec).sum).in)

 
}
