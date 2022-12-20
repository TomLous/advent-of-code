package aoc2020.day2

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line match
    case s"$min-$max $char: $password" => (PasswordPolicy(min.toInt, max.toInt, char.head), password.trim)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.count(t => t._1.isValidSimple(t._2)))

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.count(t => t._1.isValidComplex(t._2)))

}
