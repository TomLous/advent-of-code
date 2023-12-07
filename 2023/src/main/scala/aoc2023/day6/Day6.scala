package aoc2023.day6

import scala.annotation.tailrec
import scala.io.Source


object Day6 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  //

  private def parse1(lines: Iterator[String]) = lines.map{
    case s"$key: $vals" => vals.trim.split("""\s+""").map(_.toLong).toList
  }.toList match
    case t :: d :: Nil => t.zip(d)
    case _ => throw new Exception("Invalid input")

  private def parse2(lines: Iterator[String]) = lines.map{
    case s"$key: $vals" => vals.trim.replaceAll("""\s+""","").toLong
  }.toList match
    case t :: d :: Nil => (t,d)
    case _ => throw new Exception("Invalid input")

  private def isWin(hold: Long, time: Long, distance: Long) =
    val speed = hold
    val timeRemaining = time - hold
    val distanceCovered = timeRemaining * speed
    if distanceCovered > distance then 1 else 0

  private def solvewWins(in: (Long,Long)) = in match
     case (time, distance) =>

       @tailrec
       def holdToWin(hold: Long=0, wins: Int=0): Int =
          if hold >= time then wins
          else
            holdToWin(hold + 1, wins + isWin(hold, time, distance))

       holdToWin()



  private def solvePart1(inputFile: String):Long =
    val data  = parse1(readLines(inputFile))

    data.map(solvewWins).product


  private def solvePart2(inputFile: String):Long =
    val data  = parse2(readLines(inputFile))
    solvewWins(data)


  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


//  checkExample("example-part1", solvePart1)
//  println("part1: " + part1Solution)
//  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
