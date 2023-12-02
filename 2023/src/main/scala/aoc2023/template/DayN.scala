package aoc2023.template
import scala.io.Source


object DayN extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  //

  private def parse(lines: Iterator[String]) = lines


  private def solvePart1(inputFile: String):Long =
    val data  = parse(readLines(inputFile))
    0L

  private def solvePart2(inputFile: String):Long =
    val data  = parse(readLines(inputFile))
    0L

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
//  println("part1: " + part1Solution)
//  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
//  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
