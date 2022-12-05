package aoc2021.day13

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.mutable.DefaultGraphImpl

import scala.collection.immutable.HashSet
import breeze.linalg._
import breeze.numerics._

object model {

  case class Point(x: Int, y: Int)

  case class FoldInstruction(axis: Char, num: Int)

  case class Origami(state: OrigamiState, instructions: List[FoldInstruction]):
    def followInstructions(maxFolds: Option[Int]=None): OrigamiState =
      maxFolds.map(instructions.take).getOrElse(instructions).foldLeft(state){
          case (os, inst) => os.fold(inst)
        }

  object OrigamiState:
    def matrixToString(m: DenseMatrix[Boolean]): String =
      (0 until m.rows).map { row =>
        (0 until m.cols).map { col =>
          if m(row, col) then "█" else "·"
        }.mkString
      }.mkString("\n")

  case class OrigamiState(matrix: DenseMatrix[Boolean]):
    override def toString: String = OrigamiState.matrixToString(matrix)

    def fold(inst: FoldInstruction):OrigamiState =
        OrigamiState(
          inst match
            case FoldInstruction('x', num) =>
              val left = matrix(::, 0 until num)
              val right = matrix(::, matrix.cols-1 until num by -1)
              left + right
            case FoldInstruction('y', num) =>
              val top = matrix(0 until num, ::)
              val bottom = matrix(num+1 until matrix.rows, ::).t.copy(::, num-1 to 0 by -1).t.copy
              top + bottom
      )
    def count: Int = matrix.data.count(_ == true)

  object Origami:

    def apply(points: List[Point], instructions: List[FoldInstruction]):Origami =
      val maxX = (points.map(_.x).max + 1) / 2 * 2 + 1 // always odd
      val maxY = (points.map(_.y).max + 1) / 2 * 2 + 1 // always odd

      val matrix = DenseMatrix.zeros[Boolean](maxY, maxX)
      points.foreach(p => matrix(p.y, p.x) = true)
      Origami(OrigamiState(matrix), instructions)



}
