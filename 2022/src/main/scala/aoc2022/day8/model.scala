package aoc2022.day8

import breeze.generic.UFunc
import breeze.linalg.*

object model {

  case class Tree(height: Int, x: Int, y: Int)

  case class Grid(matrix: DenseMatrix[Int]):

    lazy val optimalLocationScore: Long = {
      def visible(initHeight: Int)(seq: Seq[Int]) =
        seq
          .foldLeft((0, false)) { case ((treeSeen, blocked), checkHeight) =>
            if (blocked) (treeSeen, blocked)
            else (treeSeen + 1, checkHeight >= initHeight)
          }
          ._1

      def score(height: Int, directions: List[List[Int]]): Int =
        directions.map(visible(height)).product

      (for {
        row <- 0 until matrix.rows
        col <- 0 until matrix.cols
        leftVector  = matrix(row, 0 until col).inner.toArray.toList
        rightVector = matrix(row, col + 1 until matrix.cols).inner.toArray.toList
        upVector    = matrix(0 until row, col).toArray.toList
        downVector  = matrix(row + 1 until matrix.rows, col).toArray.toList
      } yield score(matrix(row, col), List(upVector.reverse, leftVector.reverse, rightVector, downVector))).max
    }

    lazy val numVisibleFromOutside: Long =

      def visibleSet(seq: Seq[Tree]) =
        seq.tail
          .foldLeft((seq.headOption.toList, seq.head.height)) { case ((treesSoFar, curHeight), tree) =>
            if (tree.height > curHeight) (tree :: treesSoFar, tree.height)
            else (treesSoFar, curHeight)
          }
          ._1
          .toSet

      def toList(seq: Seq[(Int, Tree)]) = seq.groupBy(_._1).values.map(_.map(_._2))

      val vectors = (for {
        row <- 0 until matrix.rows
        col <- 0 until matrix.cols
        tree = Tree(matrix(row, col), row, col)
      } yield ((row, tree), (col, tree))).unzip match
        case (rowsRaw, colsRaw) => toList(rowsRaw) ++ toList(colsRaw)

      val bothSides = vectors ++ vectors.map(_.reverse)

      val visibleTrees = bothSides.flatMap(visibleSet).toSet

      visibleTrees.size

  object Grid:
    def apply(input: List[List[Int]]): Grid = Grid(DenseMatrix(input: _*))
}
