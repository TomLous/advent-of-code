package aoc2022.day21

import aoc2022.day21.model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line match
    case s"$m: $a $op $b" => MonkeyOp(Monkey(m), Monkey(a), Monkey(b), op.head)
    case s"$m: $n" => MonkeyNumber(Monkey(m), n.toInt)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable,MonkeyMath] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)
      .map(MonkeyMath.apply)

  def solvePart1(input: MonkeyMath): ZIO[Any, Throwable, BigInt] =
   
    ZIO.succeed(input.monkeyValueRoot)

  def solvePart2(input: MonkeyMath): ZIO[Any, Throwable, BigInt] =
    // TODO: implement
    ZIO.succeed(input.monkeyValueHuman)

}
