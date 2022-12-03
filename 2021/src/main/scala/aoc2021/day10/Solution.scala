package aoc2021.day10

import aoc2021.day10.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {




  def parseLine(line: String): List[Char] = line.toCharArray.toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] = lineStream.map(parseLine).map(Syntax.parseChars).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, Long] =

    ZIO.succeed(
      input
        .collect { case i: Invalid => i }
        .map {
          case Invalid(c) if c == ')' => 3
          case Invalid(c) if c == ']' => 57
          case Invalid(c) if c == '}' => 1197
          case Invalid(c) if c == '>' => 25137
        }
        .sum)

  def solvePart2(input:  List[Input]): ZIO[Any, Throwable, Long] =
    val scores = input
      .collect { case i: Incomplete => i }
      .map(_.closeSequence)
      .map(_.foldLeft(0L){
        case (total, ')') => total * 5 + 1
        case (total, ']') => total * 5 + 2
        case (total, '}') => total * 5 + 3
        case (total, '>') => total * 5 + 4
      }).sorted
    

    ZIO.succeed(scores(scores.length / 2))

}
