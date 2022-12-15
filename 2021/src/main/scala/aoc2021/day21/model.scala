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

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq

object model {

  case class PlayerState(num: Int, pos: Int, score: Int = 0) {
    lazy val boardLength: Int = 10

    def move(steps: Int): PlayerState = {
      val nextSpace = ((pos + steps - 1) % boardLength) + 1
      PlayerState(num, nextSpace, score + nextSpace)
    }
  }

  type Probability = Int
  type RollSum = Int

  type DieResult = (Die, Map[RollSum, Probability])

  trait Die {
    def rollOnce: DieResult
    def roll(times: Int):DieResult =
        (1 to times).foldLeft((this, Map.empty[RollSum, Probability])) {
        case ((die, currentMap), _) if currentMap.isEmpty => die.rollOnce
        case ((die, currentMap), _) =>
          die.rollOnce match {
            case (newDie, newMap) =>
              (
                newDie,
                (for {
                  (rollSum, prob)       <- currentMap.toList
                  (newRollSum, newProb) <- newMap.toList
                } yield (rollSum + newRollSum, prob * newProb))
                  .groupMapReduce(_._1)(_._2)(_ + _)
              )
          }
      }


  }

  case class DertiministicDie(num: Int = 0, timesRolled: Int = 0) extends Die {
    lazy val sides = 100

    override def rollOnce: DieResult =
      val value = (num + 1) % sides
      (DertiministicDie(value, timesRolled + 1), Map(value -> 1))

  }

  case object QuantumDie extends Die {
    lazy val sides = 3

    lazy val fixedProbabilities: Map[model.RollSum, model.Probability] = super.roll(sides)._2
    override def rollOnce: DieResult =
      (QuantumDie, Map(1 -> 1, 2 -> 1, 3 -> 1))

    override def roll(times: Int): DieResult =
      if(times == sides)
        (QuantumDie, fixedProbabilities)
      else
        super.roll(times)


  }

  case class GameState(playerStates: List[PlayerState], die: Die, universes: Int = 1) {
    lazy val rollsPerTurn: Int = 3
    lazy val winner            = playerStates.zipWithIndex.maxBy(_._1.score)._2

    def nextPlayerStates(playerState: PlayerState): (Die, List[(PlayerState, Int)]) = {
      die.roll(rollsPerTurn) match
        case (newDie, probabilities) =>
          (newDie, probabilities.toList.map{
            case (sum, newUniverses) =>
              (playerState.move(sum), newUniverses)
          })
    }

    lazy val nextStates: ParSeq[GameState] = {
        ???
    }
  }

  case class Game(players: List[PlayerState]) {

    def runGame(gameState: GameState, winCondition: GameState => Boolean): ParSeq[GameState] =
      if winCondition(gameState) then ParSeq(gameState)
      else gameState.nextStates.flatMap(runGame(_, winCondition))


    lazy val deterministic: GameState = {
      val initGameState                = GameState(players, DertiministicDie())
      val winCon: GameState => Boolean = gs => gs.playerStates.exists(_.score >= 1000)
      runGame(initGameState, winCon).head
    }

    lazy val quantum: List[GameState] = {
      val initGameState                = GameState(players, QuantumDie)
      val winCon: GameState => Boolean = gs => gs.playerStates.exists(_.score >= 21)
      runGame(initGameState, winCon).toList
    }
  }

}
