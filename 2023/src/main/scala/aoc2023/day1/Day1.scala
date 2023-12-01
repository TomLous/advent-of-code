package aoc2023.day1

import scala.annotation.tailrec
import scala.io.Source


object Day1 extends App:
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


  private def solution(mapping: List[(String, Int)])(input: Iterator[String]):Int =
    input.map(toDigit(mapping)).sum

  private def solvePart1(inputFile: String):Long =
    solution(Nil)(parse(readLines(inputFile)))

  private def solvePart2(inputFile: String):Long =
    val nums = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex
    solution(nums)(parse(readLines(inputFile)))

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")

  checkExample("example-part1", solvePart1)
  checkExample("example-part2", solvePart2)


  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
