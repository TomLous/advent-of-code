package aoc2023.day1

import zio.*
import zio.stream.*
import model.*

object Solution {

  def parseLine(line: String): Input = line.toCharArray.toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    val  list = input
      .map(
        _
          .filter(_.isDigit) match
            case h :: (_ :+ l) => s"$h$l".toInt
            case h :: Nil => s"$h$h".toInt
            case _ => 0
      )

    ZIO.succeed(list.sum)

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    // TODO: implement
    ZIO.succeed(0)

}
