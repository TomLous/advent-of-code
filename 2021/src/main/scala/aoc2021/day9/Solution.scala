package aoc2021.day9

import aoc2021.day9.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {


  val getEdges = ZPipeline.mapChunks[Chunk[(List[Int], Long)], DiEdge[Point]](_.flatMap(_.toList match
    case (previousLine, _) :: (currentLine, y) :: (nextLine, _) :: Nil =>
      Chunk.fromIterator(
        (None +: currentLine.zipWithIndex.map(Some(_)) :+ None).sliding(3).flatMap { case previousItem :: Some(value, pos) :: nextItem :: Nil =>
          val currentPoint = Point(value, pos.toLong, y)
          val up           = previousLine.lift(pos).map(Point(_, pos.toLong, y - 1))
          val down         = nextLine.lift(pos).map(Point(_, pos.toLong, y + 1))
          val left         = previousItem.map(_._1).map(Point(_, (pos - 1).toLong, y))
          val right        = nextItem.map(_._1).map(Point(_, (pos + 1).toLong, y))

          List(up, down, left, right).flatten.map(_.createEdge(currentPoint))
        }
      )
    case other => throw new Exception(s"Unexpected list: $other")
  ))

  def parseLine(line: String): List[Int] = line.toCharArray.map(_.asDigit).toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Graph[Point, DiEdge]] =
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

  def solvePart1(input: Graph[Point, DiEdge]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(
      input.nodes
        .filter(_.diSuccessors.isEmpty)
        .toList
        .map(_.value.value + 1)
        .sum
    )

  def solvePart2(input: Graph[Point, DiEdge]): ZIO[Any, Throwable, Long] =
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
