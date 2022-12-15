package aoc2021.day9

import aoc2021.day9.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {

  val getEdges = ZPipeline.mapChunks[Chunk[(List[Int], Long)], UnDiEdge[Point]](_.flatMap(_.toList match
    case (currentRow, y) :: (nextRow, yd) :: Nil =>
      currentRow.zipWithIndex.sliding(2).toList.flatMap { case (value, x) :: (nextVal, xr) :: Nil =>
        val currentPoint = Point(value, x, y)
        val pointR       = Point(nextVal, xr, y)
        val pointD       = Point(nextRow(x), x, yd)
        val pointRD      = Point(nextRow(xr), xr, yd)

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

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Graph[Point, UnDiEdge]] =
    for {
      edges <- (ZStream.fromChunk(Chunk(List.empty[Int])) ++ lineStream
        .map(parseLine) ++ ZStream.fromChunk(Chunk(List.empty[Int]))).zipWithIndex
        .rechunk(1)
        .sliding(3)
        .via(getEdges)
        .runCollect
        .map(_.toList)
      graph <- ZIO.succeed(Graph.from(Nil, edges))
    } yield graph

  def solvePart1(input: Graph[Point, UnDiEdge]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(
      input.nodes
        .filter(_.diSuccessors.isEmpty)
        .toList
        .map(_.value.value + 1)
        .sum
    )

  def solvePart2(input: Graph[Point, UnDiEdge]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(
      (input -- input.nodes.filter(_.value.value == 9))
        .componentTraverser()
        .map(_.nodes.size)
        .toList
        .sorted
        .takeRight(3)
        .product
    )

}
