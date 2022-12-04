package aoc2021.day11

import aoc2021.day11.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {


  val getEdges = ZPipeline.mapChunks[Chunk[(List[Int], Long)], UnDiEdge[Point]](_.flatMap(_.toList match
    case (currentRow, y) :: (nextRow, yd) :: Nil  =>
      currentRow.zipWithIndex.sliding(2).toList.flatMap {
        case (value, x) :: (nextVal, xr) :: Nil =>
          val currentPoint = Point(value, x, y)
          val pointR = Point(nextVal, xr, y)
          val pointD = Point(nextRow(x), x, yd)
          val pointRD = Point(nextRow(xr), xr, yd)

          List(
            currentPoint ~ pointR,
            currentPoint ~ pointD,
            currentPoint ~ pointRD,
            pointR ~ pointRD,
            pointD ~ pointRD,
            pointR ~ pointD
          )
        }
  ))


  def parseLine(line: String): List[Int] = line.toCharArray.map(_.asDigit).toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Grid] =
    for {
      edges <- lineStream
        .map(parseLine)
        .zipWithIndex
        .rechunk(1)
        .sliding(2)
        .via(getEdges)
        .runCollect
        .map(_.toSet)
      graph <- ZIO.succeed(Grid(edges))
    } yield graph


  def solvePart1(input: Grid): ZIO[Any, Throwable, Long] =
    val mutGraph = input.iterate(100)
    ZIO.succeed(mutGraph.flashCounter)

  def solvePart2(input: Grid): ZIO[Any, Throwable, Long] =
    val mutGraph = input.iterateUntilFlashSync
    ZIO.succeed(
      mutGraph.iterationCounter
    )

}
