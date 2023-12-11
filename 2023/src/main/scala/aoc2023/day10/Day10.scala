package aoc2023.day10

import scala.io.Source
import scala.annotation.tailrec
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.DiEdge
import scalax.collection.immutable.DefaultGraphImpl


object Day10 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  case class Point(x: Int, y: Long)

  private def parse(lines: Iterator[String]) = lines
    .zipWithIndex
    .flatMap{
      case (line, row) => line
        .toCharArray
        .zipWithIndex
        .flatMap {
          case ('.', _) => None
          case (c, col) => Some(Point(col+1, row+1) -> c)
        }
    }.toMap

  private def solve(inputFile: String, part: Int):Long =
    val pointCharMap = parse(readLines(inputFile))

    val edges = pointCharMap.foldLeft(List.empty[DiEdge[Point]]){
      case (edges, (point, char)) =>
        val north = Point(point.x, point.y - 1)
        val south = Point(point.x, point.y + 1)
        val east = Point(point.x + 1 , point.y)
        val west = Point(point.x - 1 , point.y)

        val checkPoints = char match
          case '|' => north :: south :: Nil
          case '-' => east :: west :: Nil
          case 'L' => north :: east :: Nil
          case 'J' => north :: west :: Nil
          case '7' => south :: west :: Nil
          case 'F' => south :: east :: Nil
          case _ => Nil

        checkPoints.filter(pointCharMap.contains)  match
          case a :: b :: Nil => point ~> a :: point ~> b :: edges
          case _ => edges


    }

    val startPoint = pointCharMap.find(_._2 == 'S').get._1
    val allEdges = edges ++ edges.filter(_.target == startPoint).map(p => startPoint ~> p.source)

    val graph = DefaultGraphImpl.from(allEdges)
    val startNode = graph.get(startPoint)

    @tailrec
    def cycle(currentNode: graph.NodeT, visited:List[graph.NodeT]=Nil):List[graph.NodeT] =
      currentNode.diSuccessors.diff(visited.toSet).headOption match
        case Some(next) => cycle(next, visited :+ currentNode)
        case None => visited :+ currentNode

    val loop = cycle(startNode)

    Math.ceil(loop.size / 2.0).toLong
//
//
//    startNode.findCycle match
//      case Some(cycle) =>
//        cycle.nodes.foreach(
//            node => println(node.outer.toString + ": " + pointCharMap.get(node.outer))
//        )
//        cycle.length / 2
//      case _ => throw new Exception("No cycle found")
//



  private def solvePart1(inputFile: String):Long =
    solve(inputFile, 1)

  private def solvePart2(inputFile: String):Long =
    solve(inputFile, 2)

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


//  checkExample("example-part1", solvePart1)
//  checkExample("example-part2", solvePart1)
  println("part1: " + part1Solution)
//  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
//  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
