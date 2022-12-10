package aoc2022.day10

import breeze.linalg._
import breeze.util._

object model {

  trait Input:
    def cycleLength: Int
    def action(l: Long):Long

  case class AddX(value: Int) extends Input:
    override def cycleLength: Int = 2
    override def action(l: Long): Long = l + value

  case object Noop extends Input:
    override def cycleLength: Int = 1
    override def action(l: Long): Long = l

  case class CRT(input: List[Input]):
    lazy val cycleList:List[(Int, Long)] = {
      val (_, _, list)  = input.foldLeft((1L, 0, List.empty[(Int, Long)])) {
        case ((total, prevCycle, cycleList), input) =>
          val newTotal = input.action(total)
          val newCycle = prevCycle + input.cycleLength
          val addlist = (prevCycle + 1 until newCycle).map((_, total))

          (newTotal, newCycle, cycleList ++ addlist :+ (newCycle, total))

      }
      list
    }

    lazy val run1: Long = {
      cycleList.filter(c => List(20,60,100,140,180,220).contains(c._1)).map(c => c._1 * c._2).sum
    }


    lazy val run2: Long = {
      def matrixToString(m: DenseMatrix[Boolean]): String =
        (0 until m.rows)
          .map { row =>
            (0 until m.cols).map { col =>
              if m(row, col) then "█" else "·"
            }.mkString
          }
          .mkString("\n"
          )

      val screenWidth = 40

      val pixels = cycleList.map{
        case (cycle, x) => (x until x + 3).toList.contains(cycle % screenWidth)
      }.grouped(screenWidth).toList

      val m = DenseMatrix(pixels: _*)

      println(matrixToString(m))

      0L
    }

}
