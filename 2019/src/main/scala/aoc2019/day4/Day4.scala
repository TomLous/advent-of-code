package aoc2019.day4

import scala.io.Source


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

  private def parse(lines: Iterator[String]) = lines.mkString match
    case s"$min-$max" => (min.toInt, max.toInt)

  private def increasing(digits: Seq[Int]):Boolean =
    digits.sliding(2).forall( x => x(0) <= x(1) )

  private def hasRepeat(digits: Seq[Int]):Boolean =
    digits.sliding(2).exists( x => x(0) == x(1))

  private def hasPair(digits: Seq[Int]): Boolean =
    ((-1 :: digits.toList) :+ -1).sliding(4).exists(x => x(0) != x(1) && x(1) == x(2) && x(2) != x(3))



  private def check(from: Int, to: Int, check: Seq[Int] => Boolean):Int =
    (for{
      num <- from to to
      digits = num.toString.map(_.asDigit)
      if check(digits)
    } yield num).size

  private def solvePart1(inputFile: String):Long =
    val data  = parse(readLines(inputFile))
    check(data._1, data._2, d => increasing(d) && hasRepeat(d))


  private def solvePart2(inputFile: String):Long =
    val data  = parse(readLines(inputFile))
    check(data._1, data._2, d => increasing(d) && hasPair(d))

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


//  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
