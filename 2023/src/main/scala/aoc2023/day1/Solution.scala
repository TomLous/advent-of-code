package aoc2023.day1

import aoc2023.day1.model.*
import zio.*
import zio.stream.*

import scala.annotation.tailrec

object Solution {

  def parseLine(line: String): Input = line

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(solution(Nil)(input))

  private def toDigit(mapping: List[(String, Int)])(str: String): Int =
    @tailrec
    def rec(s: String, digits:List[Int]=Nil): List[Int] =
        if s.isEmpty then digits
        else
          val newDigits =
            if s.head.isDigit then digits :+ s.head.asDigit
            else
              mapping.find(m => s.startsWith(m._1)) match
                case Some((_, digit)) => digits :+ digit
                case None             => digits
          rec(s.tail, newDigits)

    val digitList = rec(str)
    digitList.head * 10 + digitList.last


  private def solution(mapping: List[(String, Int)])(input: List[Input]):Int =
    input.map(toDigit(mapping)).sum

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, BigInt] =
    val nums = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex
    ZIO.succeed(solution(nums)(input))

}
