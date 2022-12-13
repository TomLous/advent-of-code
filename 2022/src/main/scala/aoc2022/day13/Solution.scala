package aoc2022.day13

import aoc2022.day13.model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Option[DistressSignal] =
    def rec(chars: List[Char], curList: ListOfDistressSignals): (ListOfDistressSignals, List[Char]) =
      chars match
        case Nil         => (curList, Nil)
        case ',' :: tail => rec(tail, curList)
        case '[' :: tail =>
          val (newList, remaining) = rec(tail, ListOfDistressSignals(Nil))
          rec(remaining, curList.add(newList))
        case ']' :: tail =>
          (curList, tail)
        case _ =>
          val (num, rest) = chars.span(_.isDigit)
          rec(rest, curList.add(Num(num.mkString.toInt)))

    val (list, _) = rec(line.toCharArray.toList, ListOfDistressSignals(Nil))
    list.nums.headOption

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[DistressSignal]] =
    lineStream
      .map(parseLine)
      .rechunk(1)
      .collect { case Some(signal) => signal }
      .runCollect
      .map(_.toList)

  def solvePart1(input: List[DistressSignal]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(
      input
        .grouped(2)
        .zipWithIndex
        .collect {
          case (left :: right :: Nil, index) if left.compareTo(right) < 0 => index + 1
        }
        .sum
    )

  def solvePart2(input: List[DistressSignal]): ZIO[Any, Throwable, Long] =
    val dividers = List(parseLine("[[2]]"), parseLine("[[6]]")).flatten

    val sorted = (input ++ dividers).sorted

    ZIO.succeed(
      dividers
        .map(sorted.indexOf)
        .map(_.toLong + 1L)
        .product
    )

}
