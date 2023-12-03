package aoc2023.day3

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex


object Day3 extends App:
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
    val data  = readLines(inputFile).toList
    val rows = data.length
    val cols = data.head.length

    @tailrec
    def iterate(rowC: Int=0, colC: Int=0, nums: List[Int]=Nil):List[Int] =
      if rowC >= rows then
        nums
      else if colC >= cols then
        iterate(rowC+1, 0, nums)
      else if !data(rowC).charAt(colC).isDigit then
        iterate(rowC, colC+1, nums)
      else
        def validateNum(numC: Int, numS: String="", validNum:Boolean=false):(Int,Option[Int]) =
          if numC < cols && data(rowC).charAt(numC).isDigit then
            val adjecentSymbol = (for{
                r <- -1 to 1
                c <- -1 to 1
                ch =  Try(data(rowC + r).charAt(numC + c)).toOption
                } yield ch).flatten.exists(c => !c.isDigit && c != '.')

            validateNum(numC+1, numS + data(rowC).charAt(numC), validNum || adjecentSymbol)
          else
            if numS.nonEmpty && validNum then (numC, Some(numS.toInt))
            else (numC, None)


        validateNum(colC) match
          case  (nextC, Some(num)) =>
            iterate(rowC, nextC, num :: nums)
          case  (nextC,None) =>
            iterate(rowC, nextC, nums)


    iterate().sum

  private def solvePart2(inputFile: String):Long =
    val data  = readLines(inputFile).toList
    val rows = data.length
    val cols = data.head.length

    @tailrec
    def iterate(rowC: Int=0, colC: Int=0, numsAndGear: List[(Int, List[(Int, Int)])]=Nil):List[(Int, List[(Int, Int)])] =
      if rowC >= rows then
        numsAndGear
      else if colC >= cols then
        iterate(rowC+1, 0, numsAndGear)
      else if !data(rowC).charAt(colC).isDigit then
        iterate(rowC, colC+1, numsAndGear)
      else
        def validateNum(numC: Int, numS: String="", gears:List[(Int, Int)]=Nil):(Int,Option[(Int,List[(Int, Int)])]) =
          if numC < cols && data(rowC).charAt(numC).isDigit then
            val adjecentGears = for{
                r <- -1 to 1
                c <- -1 to 1
                if Try(data(rowC + r).charAt(numC + c) == '*').toOption.getOrElse(false)
                } yield (rowC + r, numC + c)

            validateNum(numC+1, numS + data(rowC).charAt(numC), gears ++ adjecentGears)
          else
            if numS.nonEmpty && gears.nonEmpty  then (numC, Some((numS.toInt, gears.distinct)))
            else (numC, None)


        validateNum(colC) match
          case  (nextC, Some(newNumAndGears)) =>
            iterate(rowC, nextC, newNumAndGears :: numsAndGear)
          case  (nextC,None) =>
            iterate(rowC, nextC, numsAndGear)


    iterate().flatMap {
      case (num, gears) => gears.map{
        g => g -> num
      }
    }
      .groupBy(_._1)
      .values
      .filter(_.length == 2)
      .map(_.map(_._2).product)
      .sum

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
