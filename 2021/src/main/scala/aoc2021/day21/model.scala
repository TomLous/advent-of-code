package aoc2021.day21

import breeze.linalg.*
import breeze.numerics.*
import com.sun.org.apache.xml.internal.security.algorithms.Algorithm
import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {

  case class PlayerState(num: Int, pos: Int, score: Int = 0) {
    lazy val boardLength: Int = 10

    def move(steps: Int): PlayerState = {
      val nextSpace = ((pos + steps - 1) % boardLength) + 1
      PlayerState(num, nextSpace, score + nextSpace)
    }
  }

  trait Die {
    def roll: List[Die]
    def timesRolled: Int
    def num: Int
  }

  case class DertiministicDie(num: Int = 0, timesRolled: Int = 0) extends Die {
    lazy val sides = 100

    override def roll: List[DertiministicDie] =
      List(DertiministicDie((num + 1) % sides, timesRolled + 1))

  }

  case class QuantumDie(num: Int = 0, timesRolled: Int = 0) extends Die {
    lazy val sides = 3

    override def roll: List[QuantumDie] = (0 until sides).map(
      QuantumDie(_, timesRolled + 1)
    ).toList

  }

  case class GameState(playerStates: List[PlayerState], die: Die, playerTurn: Int = 0, dieRolls:List[Int] = Nil) {
    lazy val score: Long       = playerStates.map(_.score).min * die.timesRolled
    lazy val rollsPerTurn: Int = 3
    lazy val winner            = playerStates.zipWithIndex.maxBy(_._1.score)._2


    lazy val nextStates: List[GameState] = {
      die.roll.map(newDie =>
        val newDieRolls = dieRolls :+ newDie.num
        if(newDieRolls.length == rollsPerTurn){
          val currentPlayerState = playerStates(playerTurn).move(newDieRolls.sum)
//          println("Player " + (playerTurn+1) + " rolled " + newDieRolls.mkString(",") + " and moved to " + currentPlayerState.pos + " for a score of " + currentPlayerState.score)
          val newPlayerStates = playerStates.updated(playerTurn, currentPlayerState)
          val nextPlayerTurn = (playerTurn + 1) % playerStates.length
          GameState(newPlayerStates, newDie, nextPlayerTurn, Nil)
        } else {
          GameState(playerStates, newDie, playerTurn, newDieRolls)
        }
      )
    }


  }

  case class Game(players: List[PlayerState]) {

    def runGame(gameState: GameState, winCondition: GameState => Boolean): List[GameState] =
      if winCondition(gameState) then List(gameState)
      else gameState.nextStates.flatMap(runGame(_, winCondition))

    lazy val deterministic: GameState = {
      val initGameState                = GameState(players, DertiministicDie())
      val winCon: GameState => Boolean = gs => gs.playerStates.exists(_.score >= 1000)
      runGame(initGameState, winCon).head
    }

    lazy val quantum: Map[Int, Long] = {
      val winners:scala.collection.mutable.Map[Int,Long] = scala.collection.mutable.Map(0 -> 0L, 1 -> 0L)
      val initGameState                = GameState(players, QuantumDie())
      val winCon: GameState => Boolean = gs =>
        if(gs.playerStates.exists(_.score >= 21))
          if(winners(0) % 100000 == 0) println("Player 1 wins  " + winners(0))
          if(winners(1) % 100000 == 0) println("Player 2 wins  " + winners(0))
//          println("Player " + (gs.winner+1) + " wins with a score of " + gs.playerStates(gs.winner).score)
          winners(gs.winner) += 1
          true
        else
          false
      runGame(initGameState, winCon)
      winners.toMap
    }
  }

}
