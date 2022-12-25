package aoc2022.day22

import aoc2022.day12.Solution.parseLine
import aoc2022.day22.model.GameBoard
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.Implicits.*
import scalax.collection.edge.*
import zio.*
import zio.stream.*

object Solution {


  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, GameBoard] =
    lineStream
      .runCollect
      .map(_.toList)
      .map(GameBoard.apply)

  def solvePart1(input: GameBoard): ZIO[Any, Throwable, BigInt] =
    val (point, orientation) = input.wrapAround.runCommands
    val score = point.score + orientation.score
    ZIO.succeed(score)

  def solvePart2(input: GameBoard): ZIO[Any, Throwable, BigInt] =
    val (point, orientation) = input.wrapCube.runCommands
    val score = point.score + orientation.score
    ZIO.succeed(score)

}
