package aoc2021.day20

import aoc2021.day20.model.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {





  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Trench] =
    for {
      lines <- lineStream.runCollect
    } yield Trench(lines.toList)


  def solvePart1(input: Trench): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.enhance(2).lit)

  def solvePart2(input: Trench): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.enhance(50).lit)

}
