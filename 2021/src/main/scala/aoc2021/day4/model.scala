package aoc2021.day4


import scala.collection.mutable
import scala.util.Try

object model {

  case class Board(grid: List[List[Int]], horizontals: List[Int], verticals: List[Int], unmarked: List[Int]) {
    lazy val rowCols: List[(List[Int], Int)] = grid.zipWithIndex
    lazy val colRows: List[(List[Int], Int)] = grid.transpose.zipWithIndex
    lazy val gridSize                        = grid.size
    override def toString: String            = grid.map(_.map(s"%2d".format(_)).mkString(" ")).mkString("\n")

    def update(value: Int): Board = {
      val newUnmarked = unmarked.filter(_ != value)
      val newHorizontals = rowCols.map {
        case (row, rowIdx) if row.contains(value) => horizontals(rowIdx) + 1
        case (_, rowIdx)                          => horizontals(rowIdx)
      }
      val newVerticals = colRows.map {
        case (col, colIdx) if col.contains(value) => verticals(colIdx) + 1
        case (_, colIdx)                          => verticals(colIdx)
      }
      Board(grid, newHorizontals, newVerticals, newUnmarked)
    }

    val bingo: Boolean = horizontals.contains(gridSize) || verticals.contains(gridSize)

    val sum: Int = unmarked.sum
  }

  object Board:
    def apply(grid: List[List[Int]]): Board =
      val gridSize = grid.size
      Board(grid, List.fill(gridSize)(0), List.fill(gridSize)(0), grid.flatten)


  case class Result(nums: List[Int], boards: List[Board]):
    val solution: Option[Int] = nums.headOption match
      case Some(num) => boards.find(_.bingo).map(_.sum * num)
      case None => None

  object Result:
    def apply(num: Option[Int]=None, boards: List[Board]=Nil): Result = Result(num.toList, boards)

}