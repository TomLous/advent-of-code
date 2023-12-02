package aoc2019.day2

import scala.collection.mutable.ListBuffer
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

  private def parse(lines: Iterator[String]) = lines.mkString.split(",").map(_.toLong)

  private def runProgram(source: Seq[Long], input1: Long, input2: Long): List[Long] =
    val data = ListBuffer.from(source)
    data(1) = input1
    data(2) = input2
    var pc = 0
    while data(pc) != 99 do
      val op = data(pc)
      val a  = data(pc + 1).toInt
      val b  = data(pc + 2).toInt
      val c  = data(pc + 3).toInt
      data(c) = op match
        case 1 => data(a) + data(b)
        case 2 => data(a) * data(b)
        case _ => throw new Exception(s"Invalid opcode $op")
      pc += 4
    data.toList

  private def solvePart1(inputFile: String): Long =
    runProgram(parse(readLines(inputFile)), 12, 2).head

  private def solvePart2(inputFile: String): Long =
    val data = parse(readLines(inputFile))

    (for {
      i1 <- 0 to 99
      i2 <- 0 to 99
      if runProgram(data, i1, i2).head == 19690720
    } yield 100 * i1 + i2).head

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")

//  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
