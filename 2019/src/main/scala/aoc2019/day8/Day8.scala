package aoc2019.day8

import scala.io.Source
import scala.annotation.tailrec


object Day8 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")


  private def parse(lines: Iterator[String]) = lines.flatMap(_.toCharArray).map(_.asDigit).grouped(25*6).toList




  private def solvePart1(inputFile: String):Long =
    val layers = parse(readLines(inputFile))
    val zeroLayer = layers.map(layer => (layer, layer.count(_ == 0))).minBy(_._2)._1
    zeroLayer.count(_ == 1) * zeroLayer.count(_ == 2)

  private def solvePart2(inputFile: String):Long =
    val layers = parse(readLines(inputFile))

    val finalLayer = layers.foldLeft(layers.head)((sumLayer, layer) =>
        sumLayer.zip(layer).map{ case (a,b) => if a == 2 then b else a }
    )
    finalLayer.grouped(25).foreach(row => println(row.map{ case 0 => " " case 1 => "█" }.mkString)) // ·
    0L


  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


//  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
//  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
