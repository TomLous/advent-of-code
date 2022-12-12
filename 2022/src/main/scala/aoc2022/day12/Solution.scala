package aoc2022.day12

import aoc2022.day12.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {

  val getEdges = ZPipeline.mapChunks[Chunk[(List[Char], Long)], DiEdge[Point]](_.flatMap(_.toList match
    case (currentRow, y) :: (nextRow, yd) :: Nil =>
      currentRow.zipWithIndex.sliding(2).toList.flatMap { case (height, x) :: (nextHeight, xr) :: Nil =>
        val currentPoint = Point(height, x, y)
        val pointR       = Point(nextHeight, xr, y)
        val pointD       = Point(nextRow(x), x, yd)
        val pointRD      = Point(nextRow(xr), xr, yd)

        List(
          if currentPoint.canReach(pointR) then Some(currentPoint ~> pointR) else None,
          if pointR.canReach(currentPoint) then Some(pointR ~> currentPoint) else None,
          if currentPoint.canReach(pointD) then Some(currentPoint ~> pointD) else None,
          if pointD.canReach(currentPoint) then Some(pointD ~> currentPoint) else None,
          if pointR.canReach(pointRD) then Some(pointR ~> pointRD) else None,
          if pointRD.canReach(pointR) then Some(pointRD ~> pointR) else None,
          if pointD.canReach(pointRD) then Some(pointD ~> pointRD) else None,
          if pointRD.canReach(pointD) then Some(pointRD ~> pointD) else None,
        ).flatten
      }
  ))

  def parseLine(line: String): List[Char] = line.toCharArray.toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Hill] =
    for {
      edges <- lineStream
        .map(parseLine)
        .zipWithIndex
        .rechunk(1)
        .sliding(2)
        .via(getEdges)
        .runCollect
        .map(_.toSet)
      hill <- ZIO.succeed(Hill(edges))
    } yield hill

  def solvePart1(input: Hill): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.getShortestPathSize)

  def solvePart2(input: Hill): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.getAnyAShortestPathSize)

}
