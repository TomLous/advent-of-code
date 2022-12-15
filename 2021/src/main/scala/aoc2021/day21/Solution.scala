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
    val score = input.deterministic match
      case GameState(ps, d:DertiministicDie, _) => ps.map(_.score).min * d.timesRolled
    
    ZIO.succeed(score)

  def solvePart2(input: Game): ZIO[Any, Throwable, Long] =
    val gs = input.quantum
    println(gs.size)
    val numWins = gs.groupBy(_.winner).map(_._2.map(_.universes.toLong).sum)
    numWins.foreach(println)
//      .groupMapReduce(identity)(_ => 1)(_ + _)
//      .toList
//      .sortBy(-_._2)
//      map(_.playerScores.head)


    ZIO.succeed(numWins.max)

}
