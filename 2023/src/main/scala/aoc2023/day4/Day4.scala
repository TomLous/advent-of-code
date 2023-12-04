package aoc2023.day4

import spire.random.DistIterator

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.convert.JavaCollectionWrappers
import scala.collection.parallel.Splitter
import scala.io.Source
import scala.util.matching.Regex


object Day4 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  //

  private def splitNums(str: String) =
    str.trim
      .split("""\s+""").map(_.trim.toInt).toSet

  private def parse(lines: Iterator[String]) = lines.map{
    case s"Card $gameNumber: $winning | $my" => gameNumber.trim.toInt -> splitNums(my).intersect(splitNums(winning))
  }.toMap


  private def solvePart1(inputFile: String):Long =
    val data  = parse(readLines(inputFile))
    data.values.map(winningNums => math.pow(2, winningNums.size-1).toLong).sum

  private def solvePart2(inputFile: String):Long =
    val data  = parse(readLines(inputFile))


    def countWins(pos: Int, current: Set[Int]):Long =
      val numWon = current.size
      if numWon == 0 then 1
      else
        (pos+1 to pos+numWon).map(p =>
          countWins(p, data(p))
        ).sum + 1

    data.map{
      case (o, d) =>  countWins(o, d)
    }.sum

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
