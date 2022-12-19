package aoc2022.day17

import breeze.linalg.*

import scala.annotation.tailrec

object model {

  case class Tetris(jetStream: List[Char], initMatrix: DenseMatrix[Int] = DenseMatrix.zeros[Int](0, 7), groundHeight:Long=0) {
    private lazy val width             = initMatrix.cols
    private val shapeAppearAboveGround = 3
    private val shapeAppearFromLeft    = 2
    private val shapes = List(
      DenseMatrix(
        (1, 1, 1, 1)
      ),
      DenseMatrix(
        (0, 1, 0),
        (1, 1, 1),
        (0, 1, 0)
      ),
      DenseMatrix(
        (0, 0, 1),
        (0, 0, 1),
        (1, 1, 1)
      ),
      DenseMatrix(
        1,
        1,
        1,
        1
      ),
      DenseMatrix(
        (1, 1),
        (1, 1)
      )
    )

    def towerHeight(matrix: DenseMatrix[Int]): Long  =
      matrix(*, ::).foldLeft(matrix.rows){
        case (height, row) if row.forall(_ == 0) => height - 1
        case (height, _) => height
      }


    def topFilledRow(matrix: DenseMatrix[Int]): Int =
      matrix.findAll(_ == 2).map(_._1).minOption.getOrElse(matrix.rows)

//    lazy val groundHeight: Long = (initMatrix.rows - initMatrix.findAll(_ == 2).map(_._1).minOption.getOrElse(0)).toLong

    def addShape(matrix: DenseMatrix[Int], shape: DenseMatrix[Int]): DenseMatrix[Int] = {
      val shapeHeight       = shape.rows
      val groundRowInMatrix = topFilledRow(matrix)
      val groundHeight      = matrix.rows - groundRowInMatrix
      val newHeight         = shapeHeight + shapeAppearAboveGround + groundHeight
      val newMatrix         = DenseMatrix.zeros[Int](newHeight, matrix.cols)
      newMatrix(newMatrix.rows - groundHeight until newMatrix.rows, 0 until width)              := matrix(groundRowInMatrix until matrix.rows, ::)
      newMatrix(0 until shape.rows, shapeAppearFromLeft until shape.cols + shapeAppearFromLeft) := shape
      newMatrix
    }

    def moveShape(matrix: DenseMatrix[Int], rowMod: Int = 0, colMod: Int = 0): (DenseMatrix[Int], Boolean) = {
      val currentShapePos = matrix.findAll(_ == 1)
      val newShapePos = currentShapePos.map { case (row, col) =>
        (row + rowMod, col + colMod)
      }
      val moved =
        if (
          newShapePos.forall { case (newRow, newCol) =>
            newRow >= 0 && newRow < matrix.rows &&
            newCol >= 0 && newCol < matrix.cols &&
            matrix(newRow, newCol) < 2
          }
        ) {
          currentShapePos.foreach { case (row, col) =>
            matrix(row, col) = 0
          }
          newShapePos.foreach { case (row, col) =>
            matrix(row, col) = 1
          }
          true
        } else {
          false
        }

      (matrix, moved)
    }

    def followInstruction(matrix: DenseMatrix[Int], instruction: Char): DenseMatrix[Int] =
      instruction match {
        case '>' => moveShape(matrix, colMod = 1)._1
        case '<' => moveShape(matrix, colMod = -1)._1
      }

    def restMatrix(matrix: DenseMatrix[Int]): DenseMatrix[Int] = {
      matrix.findAll(_ == 1).foreach { case (row, col) =>
        matrix(row, col) = 2
      }
      matrix
    }

    @tailrec
    private def step(matrix: DenseMatrix[Int], instructions: List[Char]): (DenseMatrix[Int], List[Char]) = {
      val endlessInstructions    = if instructions.isEmpty then jetStream else instructions
      val (newMatrix, movedDown) = moveShape(followInstruction(matrix, endlessInstructions.head), 1)
      if (!movedDown) (matrix, endlessInstructions.tail)
      else step(newMatrix, endlessInstructions.tail)
    }

    def loop(rockNum: Long, matrix: DenseMatrix[Int], input: List[Char]): (Long, DenseMatrix[Int], List[Char], Long) =
      val matrixWithShape         = addShape(matrix, shapes((rockNum % shapes.length).toInt))
      val (movedMatrix, newInput) = step(matrixWithShape, input)
      val restedMatrix = restMatrix(movedMatrix)
      val height = towerHeight(restedMatrix)
      (rockNum+1, restedMatrix, newInput, height)

    def recurse(matrix: DenseMatrix[Int], input: List[Char], rockNum: Long, max: Long, height: Long=0): (DenseMatrix[Int], List[Char], Long) =
      if (rockNum == max) {
        (matrix, input, height)
      } else {
        val (newRocknum, newMatrix, newInput, newHeight) = loop(rockNum, matrix, input)
        recurse(newMatrix, newInput, newRocknum, max, newHeight)
      }

    def run(numRocks: Long): Tetris = {
      val maxTry = 4000
      val lazyList = LazyList.iterate((0L, initMatrix, jetStream, 0L)) { case (matrix, input, rockNum, _) => loop(matrix, input, rockNum)}
      val heights = lazyList.map(_._4)
      val heightDeltas = heights.lazyZip(heights.tail).map(c => c._2 - c._1).take(maxTry).toList

      def findPattern(startPos: Int): (Int, List[Long]) =
        if(startPos > maxTry) throw Exception("No loop found")
        val heightDeltasFrom = heightDeltas.drop(startPos-1).toList

        def loop(len: Int): Option[List[Long]] =
          if(len + startPos > maxTry) None
          val checkPattern = heightDeltasFrom.take(len).toList
          val findPatternAgain = heightDeltasFrom.drop(len).sliding(len).map(_.toList).find(_ == checkPattern).getOrElse(Nil)
          if(findPatternAgain.isEmpty) None
          else if (findPatternAgain ++ findPatternAgain == heightDeltasFrom.take(len*2).toList) Some(checkPattern)
          else loop(len+1)
        loop(10) match
          case Some(pattern) => (startPos, pattern)
          case None => findPattern(startPos+1)

      val (startPos, pattern) = findPattern(1)


      val start = heightDeltas.take(startPos-1).toList


      val l1 = (numRocks - startPos)
      val factor = l1 / pattern.length
      val rest = l1 % pattern.length
      val end = pattern.take(rest.toInt +1)
      val result = start.sum + (pattern.sum * factor) + end.sum
      
      Tetris(jetStream, initMatrix, result)
    }

    def matrixToString(m: DenseMatrix[Int], maxRows: Option[Int] = None): String =
      "━".repeat(m.cols) + "\n" +
        ((0 until maxRows.map(_ min m.rows).getOrElse(m.rows))
          .map { row =>
            (0 until m.cols).map { col =>
              val v = m(row, col)
              if v == 1 then "#"
              else if v == 2 then "█"
              else "·"
            }.mkString
          }
          .mkString("\n")) + "\n" + "━".repeat(m.cols) + "\n"

    override def toString: String = matrixToString(initMatrix)
  }
}
