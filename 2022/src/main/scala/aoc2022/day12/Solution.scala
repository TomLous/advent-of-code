package aoc2022.day12

import aoc2022.day12.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {

  def cornerEdges(cornerPoint: Point, otherPoints: List[Point]):List[DiEdge[Point]] =
    otherPoints.foldLeft(List.empty[DiEdge[Point]]){
      case (l, point) =>
        List(
          if point.canReach(cornerPoint) then Some(point ~> cornerPoint) else None,
          if cornerPoint.canReach(point) then Some(cornerPoint ~> point) else None
        ).flatten ::: l
     }

  val getEdges = ZPipeline.mapChunks[Chunk[(List[Char], Long)], DiEdge[Point]](_.flatMap(_.toList match
    case (currentRow, y) :: (nextRow, yd) :: Nil =>
      currentRow.zipWithIndex.sliding(2).toList.flatMap { case (height, x) :: (nextHeight, xr) :: Nil =>
        val currentPoint = Point(height, x, y)
        val pointR       = Point(nextHeight, xr, y)
        val pointD       = Point(nextRow(x), x, yd)
        val pointRD      = Point(nextRow(xr), xr, yd)

        cornerEdges(currentPoint, List(pointR, pointD)) ++ cornerEdges(pointRD, List(pointR, pointD))
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
