package aoc2019.day3

import scala.io.Source


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

  private enum Direction(val n: Int, val vert: Int, val hor: Int):
    def move(p: Point, step: Long): Seq[(Long,Point)] =
      for{
        i <- 0 to n
      } yield i.toLong + step -> Point(p.x + i * vert, p.y + i * hor)

    case Up(override val n: Int) extends Direction(n, 1, 0)
    case Down(override val n: Int) extends Direction(n, -1, 0)
    case Left(override val n: Int) extends Direction(n, 0, -1)
    case Right(override val n: Int) extends Direction(n, 0, 1)

  case class Point(x: Long, y: Long):
    lazy val manhattanDistance = x.abs + y.abs

  private def parse(lines: Iterator[String]) = lines.map(_.split(',').toList.map{
    case s"R$n" => Direction.Right(n.toInt)
    case s"L$n" => Direction.Left(n.toInt)
    case s"U$n" => Direction.Up(n.toInt)
    case s"D$n" => Direction.Down(n.toInt)
  }).toList match
    case w1::w2::Nil => (w1, w2)
    case _ => throw new IllegalArgumentException("Invalid input")

  private def pathToPointSet(path: List[Direction]): Map[Long,Point]=
    val firstPoint = (0L,Point(0,0))
    path.foldLeft(firstPoint, List.empty[(Long, Point)]){
      case (((step,offsetPoint), points), direction) =>
        val newPoints = direction.move(offsetPoint, step)
        (newPoints.last,  points ++ newPoints)
    }._2.toMap - firstPoint._1

  private def solve(inputFile: String): (Map[Long,Point], Map[Long,Point]) =
    val data  = parse(readLines(inputFile))
    (pathToPointSet(data._1), pathToPointSet(data._2))

  private def cross(path1: Map[Long,Point], path2: Map[Long,Point]): Set[Point] =
    path1.values.toSet.intersect(path2.values.toSet)



  private def solvePart1(inputFile: String):Long =
    val (path1, path2) = solve(inputFile)
    val crossPoints = cross(path1, path2)
    crossPoints.map(_.manhattanDistance).min

  private def solvePart2(inputFile: String):Long =
    val (path1, path2) = solve(inputFile)
    val crossPoints = cross(path1, path2)
    crossPoints.map(p => path1.find(_._2 == p).get._1 + path2.find(_._2 == p).get._1).min


  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
  checkExample("example-part2", solvePart1)
  checkExample("example-part3", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
