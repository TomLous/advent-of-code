package aoc2022.day2

import aoc2022.day2.model.*
import aoc2022.day2.model.GameAction.*
import aoc2022.day2.model.GameResult.*
import zio.*
import zio.stream.*

object Solution {


  def parseLine(line: String): Input = line.toCharArray.toList match
    case a :: _ ::  b :: Nil => (a, b)
    case _ => throw new Exception(s"Invalid input: $line")

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, Long] =
    val mapping = Map(
      'A' -> Rock,
      'B' -> Paper,
      'C' -> Scissors,
      'X' -> Rock,
      'Y' -> Paper,
      'Z' -> Scissors
    )
    
    ZIO.succeed(
      input
      .map(Strategy(mapping))
        .map(_.play)
        .sum
    )

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, Long] =
    val mappingAction = Map(
      'A' -> Rock,
      'B' -> Paper,
      'C' -> Scissors,
    )

    val mappingResult = Map(
      'X' -> Lose,
      'Y' -> Draw,
      'Z' -> Win
    )
    
    ZIO.succeed(
      input
        .map(Strategy(mappingAction, mappingResult))
        .map(_.play)
        .sum
    )

}
