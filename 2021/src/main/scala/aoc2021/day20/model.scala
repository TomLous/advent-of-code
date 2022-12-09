package aoc2021.day20

import breeze.linalg.*
import breeze.numerics.*
import com.sun.org.apache.xml.internal.security.algorithms.Algorithm
import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {

  case class Image(matrix: DenseMatrix[Int], infinite: Int = 0):
    lazy val lit: Int = {
      val allItems = matrix.toArray
      println(" - - - - - ")
//      println(matrix)
//      println(allItems.length)
//      println(allItems.sum)
      matrix.toArray.sum
  }

    val extend = 3

    lazy val expandedImage: DenseMatrix[Int] = {
      val large = DenseMatrix.fill[Int](matrix.rows + (extend * 2), matrix.cols + (extend * 2))(infinite)
      large(extend until (matrix.rows + extend), extend until (matrix.cols + extend)) := matrix
      large
    }


    def enhance(algorithm: List[Int], infinite: Int = 0): Image =
      val enhancedMatrix = DenseMatrix.fill[Int](matrix.rows + extend, matrix.cols + extend)(infinite)
      for {
        x <- 0 until enhancedMatrix.cols
        y <- 0 until enhancedMatrix.rows
        inputMatrix = expandedImage(x until x + 3, y until y + 3).t
//        _ = println(s"-> $x $y")
//        _ = println(inputMatrix)
        inputMatrixBin = inputMatrix.toArray.mkString("")
//        _ = println(inputMatrixBin)
        algoIndex = Integer.parseInt(inputMatrixBin, 2)
        _ = enhancedMatrix(x, y) = algorithm(algoIndex)
//      algoIndex = Integer.parseInt(
//        expandedImage(x until x + 3, y until y + 3).t.toArray
//          .mkString(""),
//        2
//      )
//      _ = enhancedMatrix(x, y) = algorithm(algoIndex)
      } yield ()

//      val i = enhancedMatrix(0 until 3, 0 until 3)

//      val infinite = algorithm(Integer.parseInt(i.t.toArray.mkString(""), 2))

      Image(enhancedMatrix, 0)

  case class Trench(image: Image, algorithm: List[Int]):
    def enhance(num: Int): Image = (0 until num).foldLeft(image)((img, i) => {
      println(" -- " + i)
      val e = img.enhance(algorithm)
      println(e)
      e
    })


  object Trench {
    def apply(list: List[String]): Trench = {
      val algorithm = list.head.toCharArray.map {
        case '#' => 1
        case '.' => 0
      }.toList

      val image = Image(
        DenseMatrix(
          list.tail.tail
            .map(_.toCharArray.map {
              case '#' => 1
              case '.' => 0
            })
            .toArray: _*
        ), 0
      )

      Trench(image, algorithm)
    }
  }

}
