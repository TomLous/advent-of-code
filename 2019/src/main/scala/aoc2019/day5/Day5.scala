package aoc2019.day5

import aoc2019.intcode.IntCode
import aoc2019.intcode.IntCode.*

import scala.io.Source


object Day5 extends App:
  private def readSource(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt")
  private def readLines(inputFile: String) = readSource(inputFile).getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")


  private def solvePart1(inputFile: String):Long =
    val program = read(readSource(inputFile))
    execute(Input(program, List(1))).sig.dropWhile(_ == 0).head

  private def solvePart2(inputFile: String):Long =
    val program = read(readSource(inputFile))
    execute(Input(program, List(5))).sig.head


  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


//  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
//  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
