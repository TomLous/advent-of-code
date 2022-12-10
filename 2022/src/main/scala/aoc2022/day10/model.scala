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
    val screenWidth: Int = 40

    lazy val cycleList:List[(Int, Long)] = {
      val (_, _, list)  = input.foldLeft((1L, 0, List.empty[(Int, Long)])) {
        case ((prevTotal, prevCycle, cycleList), input) =>
          val newCycle = prevCycle + input.cycleLength
          val addlist = (prevCycle + 1 to newCycle).map((_, prevTotal))

          (input.action(prevTotal), newCycle, cycleList ++ addlist)
      }
      list
    }

    lazy val signalStrength: Long =
      cycleList
        .filter(c => (c._1 - 20) % screenWidth == 0)
        .map(c => c._1 * c._2)
        .sum

    lazy val printCRT: Long = {
      def matrixToString(m: DenseMatrix[Boolean]): String =
        (0 until m.rows)
          .map { row =>
            (0 until m.cols).map { col =>
              if m(row, col) then "█" else "·"
            }.mkString
          }
          .mkString("\n"
          )



      val pixels = cycleList.map{
        case (cycle, x) => (x until x + 3).toList.contains(cycle % screenWidth)
      }.grouped(screenWidth).toList

      println(matrixToString(DenseMatrix(pixels: _*)))

      // No output, just a visualisation
      0L
    }

}
