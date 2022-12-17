package aoc2022.day17

import breeze.linalg.*

import scala.annotation.tailrec

object model {

  case class Tetris(jetStream: List[Char], initMatrix: DenseMatrix[Int] = DenseMatrix.zeros[Int](0, 7)) {
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
    
    lazy val groundHeight: Int = initMatrix.rows - initMatrix.findAll(_ == 2).map(_._1).minOption.getOrElse(0)

    def addShape(matrix: DenseMatrix[Int], shape: DenseMatrix[Int]): DenseMatrix[Int] = {
      val shapeHeight = shape.rows
//      println("shapeHeight: " + shapeHeight)
      val groundRowInMatrix = matrix.findAll(_ == 2).map(_._1).minOption.getOrElse(0)
//      println("groundRowInMatrix: " + groundRowInMatrix)
      val groundHeight = matrix.rows - groundRowInMatrix
//      println("groundHeight: " + groundHeight)
      val newHeight = shapeHeight + shapeAppearAboveGround + groundHeight
//      println("newHeight: " + newHeight)
      val newMatrix = DenseMatrix.zeros[Int](newHeight, matrix.cols)
//      println(s"matrix rows: ${matrix.rows}, cols: ${matrix.cols}")
//      println(s"newmatrix rows: ${newMatrix.rows}, cols: ${newMatrix.cols}")
//      println(s"matrix rows to get ${groundHeight until matrix.rows}")
//      println(s"newmatix rows to paste ${newMatrix.rows - groundHeight until newMatrix.rows}")
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
      val endlessInstructions = if instructions.isEmpty then jetStream else instructions
      val (newMatrix, movedDown) = moveShape(followInstruction(matrix, endlessInstructions.head), 1)
      if (!movedDown) (matrix, endlessInstructions.tail)
      else step(newMatrix, endlessInstructions.tail)
    }

    def run(numRocks: Int): Tetris = {
      val (resMatrix, inp) = (0 until numRocks).foldLeft((initMatrix, jetStream)) { case ((matrix, input), rockNum) =>
        val matrixWithShape = addShape(matrix, shapes(rockNum % shapes.length))
        val (movedMatrix, newInput) = step(matrixWithShape, input)
        val rested                  = restMatrix(movedMatrix)

//        println(matrixToString(rested))
        (rested, newInput)
      }

      Tetris(inp, resMatrix)
    }


    def matrixToString(m: DenseMatrix[Int]): String =
      "━".repeat(m.cols) + "\n" +
        ((0 until m.rows)
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
