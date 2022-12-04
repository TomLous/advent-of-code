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



  case class Origami(matrix: DenseMatrix[Boolean], instructions: List[FoldInstruction]):

    def followInstructions: Origami =
      Origami(instructions.foldLeft(matrix){
          case (m, inst) => Origami.fold(m, inst)
        }, Nil)

    def count: Int = matrix.data.count(_ == true)

  object Origami:

    def apply(points: List[Point], instructions: List[FoldInstruction]):Origami =
      val maxX = points.map(_.x).max
      val maxY = points.map(_.y).max
      val matrix = DenseMatrix.zeros[Boolean](maxY + 1, maxX + 1)
      points.foreach(p => matrix(p.y, p.x) = true)
      Origami(matrix, instructions)

    def fold(matrix: DenseMatrix[Boolean], inst: FoldInstruction):DenseMatrix[Boolean] =
      val maxX = matrix.cols
      val maxY = matrix.rows
      inst match
        case FoldInstruction('x', num) =>
          val left = matrix(::, 0 until num)
          val right = matrix(::, maxX-1 until num by -1)
          val sum = left + right
          sum
        case FoldInstruction('y', num) =>
          println(inst)
          val top = matrix(0 until num, ::)
          val bottom = matrix(num+1 until maxY, ::).t.copy(::, num-1 to 0 by -1).t.copy
          val sum = top + bottom
          sum









}
