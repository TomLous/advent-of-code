package aoc2023.template

import zio.*
import zio.stream.*
import model.*

object Solution {

  def parseLine(line: String): Input = () // TODO: implement

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    // TODO: implement
    ZIO.succeed(0)

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    // TODO: implement
    ZIO.succeed(0)

}
