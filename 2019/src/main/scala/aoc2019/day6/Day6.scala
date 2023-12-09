package aoc2019.day6

import scala.io.Source
import scalax.collection.edges.UnDiEdgeImplicits
import scalax.collection.edges.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.immutable.DefaultGraphImpl

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

  private def parse(lines: Iterator[String]) = lines.map{
    case s"$parent)$child" => parent ~ child
  }.toList





  private def solvePart1(inputFile: String):Long =
    val graph= DefaultGraphImpl.from(parse(readLines(inputFile)))
    graph
      .get("COM")
      .innerNodeTraverser
      .foldLeft(Map.empty[graph.NodeT, Long])(
        (distances, node) =>
          val lowestDepth = node
            .diPredecessors
            .flatMap(distances.get)
            .minOption
            .map(_ + 1)
            .getOrElse(0L)
          distances + (node -> lowestDepth)
      )
      .values
      .sum


  private def solvePart2(inputFile: String):Long =
    val graph= DefaultGraphImpl.from(parse(readLines(inputFile)))
    graph
      .get("YOU")
      .shortestPathTo(graph.get("SAN"))
      .map(_.length.toLong - 2)
      .getOrElse(0L)

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
