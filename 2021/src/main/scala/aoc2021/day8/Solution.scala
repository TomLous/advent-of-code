package aoc2021.day8

import aoc2021.day8.model.*
import zio.*
import zio.stream.ZStream

object Solution {


  def parseLine(str: String):(DigitIndex, DigitNumber) = str
    .split("\\|") match {
    case Array(indexStr, digitsStr) =>
      val index = DigitIndex(indexStr.trim.split(" +").toList)
      val digits = DigitNumber(digitsStr.trim.split(" +").map(Digit(_, index)).toList)
      (index, digits)
    case _ => throw new Exception("Invalid input")
  }


  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[(DigitIndex, DigitNumber)]] = lineStream.map(parseLine).runCollect.map(_.toList)


  def solvePart1(input: List[(DigitIndex, DigitNumber)]): ZIO[Any, Throwable, Long] = {
    val digitCounts = input.flatMap(_._2.digits).groupBy(identity).map{
      case (Digit(i), digits) => (i, digits.size)
    }

    val sum1478 = digitCounts(1) +  digitCounts(4) + digitCounts(7) + digitCounts(8)

    ZIO.succeed(sum1478.toLong)
  }

  def solvePart2(input: List[(DigitIndex, DigitNumber)]): ZIO[Any, Throwable, Long] = {
    ZIO.succeed(input.map(_._2.value.toLong).sum)
  }

}
