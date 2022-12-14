package aoc2022.day14

import breeze.generic.UFunc
import breeze.linalg.*
import breeze.linalg.operators.*
import breeze.numerics.*

import scala.util.*

object model {

  case class Point(x: Int, y:  Int, value: Int)

  case class Cave(matrix: DenseMatrix[Int]):
    override def toString: String = matrixToString(matrix)

    lazy val addFloor:Cave =
      val newMatrix = matrix.copy
      newMatrix(matrix.rows - 1, ::) := 1
      Cave(newMatrix)

    lazy val removeSand:Cave =
      val removeSandF = UFunc[Int, Int](i => if i == 2 then 0 else i)
      val newMatrix = matrix.copy
      Cave(removeSandF(newMatrix))

    def matrixToString(m: DenseMatrix[Int]): String =
        (0 until m.rows)
          .map { row =>
            (0 until m.cols).map { col =>
              if m(row, col) == 1 then "█"
              else if  m(row, col) == 2 then "o"
              else "·"
            }.mkString
          }
          .mkString("\n"
          )



    def dropSandFrom(sandOutlet: Point):List[Point] =
      def dropSand:Option[Point] ={
        def rec(currentX: Int, currentY: Int):Option[Point] = {
          if currentY + 1 == matrix.rows then
            None // next is off the chart, only happens without a floor (part1)
          else
            if(matrix(currentY+1, currentX) > 0) // down is blocked
              val leftDown = matrix(currentY+1, currentX-1)
              val rightDown = matrix(currentY+1, currentX+1)
              if(leftDown > 0 && rightDown > 0) then
                Some(Point(currentX, currentY, 2)) // we have a stable point
              else if(leftDown > 0)
                rec(currentX + 1, currentY+1) // we have to go right down
              else
                rec(currentX - 1, currentY+1) // we can go left down
            else // down is free
              rec(currentX, currentY+1)
        }
        rec(sandOutlet.x, sandOutlet.y) match
          case Some(p) if p == sandOutlet =>
            matrix(p.y, p.x) = p.value
            None
          case Some(p) =>
            matrix(p.y, p.x) = p.value
            Some(p)
          case None =>
            None

      }


      LazyList.continually(dropSand).takeWhile(_.isDefined).flatten.toList



  object Cave:
    def apply(wallLines: List[List[Point]]): Cave =
      val width = wallLines.map(_.map(_.x).max).max + 1 + 200
      val height = wallLines.map(_.map(_.y).max).max + 1 + 2

      val m = DenseMatrix.zeros[Int](height, width)

      wallLines.foreach(wall =>
        wall.sliding(2).foreach { case List(pFrom, pTo) =>
          if (pFrom.x == pTo.x && pFrom.y < pTo.y) m(pFrom.y to pTo.y, pFrom.x) := 1
          else if (pFrom.x == pTo.x) m(pTo.y to pFrom.y, pFrom.x) := 1
          else if (pFrom.y == pTo.y && pFrom.x < pTo.x) m(pFrom.y, pFrom.x to pTo.x) := 1
          else if (pFrom.y == pTo.y ) m(pFrom.y, pTo.x to pFrom.x) := 1
          else throw new Exception("Invalid wall")
      })

      Cave(m)


}
