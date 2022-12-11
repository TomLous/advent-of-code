package aoc2021.day21

import aoc2021.day21.model.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {


  def parseLine(line: String): PlayerState = line match
    case s"Player $num starting position: $pos" => PlayerState(num.toInt, pos.toInt)


  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Game] =
    for {
      players <- lineStream.map(parseLine).runCollect
    } yield Game(players.toList)


  def solvePart1(input: Game): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.deterministic.score)

  def solvePart2(input: Game): ZIO[Any, Throwable, Long] =
    val numWins = input.quantum.toList.sortBy(-_._2).head._2
//      .groupMapReduce(identity)(_ => 1)(_ + _)
//      .toList
//      .sortBy(-_._2)
//      map(_.playerScores.head)


    ZIO.succeed(numWins)

}
