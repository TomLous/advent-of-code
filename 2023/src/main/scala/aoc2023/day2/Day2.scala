package aoc2023.day2
import scala.io.Source

object Day2 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.', '/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f: String => Long): Unit =
    val solution = f(inputFile)
    val target   = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int): Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  //

  private def parse(lines: Iterator[String]) = lines.map { case s"Game $game: $sets" =>
    game.toLong -> sets
      .split("; ")
      .map(set =>
        set
          .split(", ")
          .map { case s"$num $color" =>
            color -> num.toInt
          }
          .toMap
      )
      .toList
  }

  private def solvePart1(inputFile: String): Long =
    val games  = parse(readLines(inputFile))
    val maxMap = Map("red" -> 12, "green" -> 13, "blue" -> 14)
    games
      .map((num, game) =>
        num -> game
          .flatMap(_.toList)
          .groupMapReduce(_._1)(_._2)(_ max _)
          .exists { case (color, max) =>
            maxMap(color) < max
          }
      )
      .filter(_._2 == false)
      .map(_._1)
      .sum

  private def solvePart2(inputFile: String): Long =
    val games = parse(readLines(inputFile))
    games
      .map(
        _._2
          .flatMap(_.toList)
          .groupMapReduce(_._1)(_._2.toLong)(_ max _)
          .values
          .product
      )
      .sum

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")

  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
