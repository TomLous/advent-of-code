package aoc2021.day12

import aoc2021.day12.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {
  
  def parseLine(line: String): UnDiEdge[Cave] = line match
    case s"$a-$b" => Cave(a) ~ Cave(b)
    case _ => throw new IllegalArgumentException(s"Invalid line: $line")

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, CaveSystem] =
    for {
      edges <- lineStream
        .map(parseLine)
        .rechunk(1)
        .runCollect
        .map(_.toSet)
      graph <- ZIO.succeed(CaveSystem(edges))
    } yield graph


  def solvePart1(input: CaveSystem): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.findPaths().size)

  def solvePart2(input: CaveSystem): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.findPaths(true).size)

}
