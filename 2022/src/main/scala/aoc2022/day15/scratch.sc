case class Point(row: Int, col: Int)

import breeze.linalg.*
import breeze.linalg.operators.*
import breeze.numerics.*

val point = Point(10, 9)
val range = 5
val maxRange = 7

def matrixToString(m: DenseMatrix[Int]): String =
  (0 until m.rows)
    .map { row =>
      (0 until m.cols).map { col =>
        if m(row, col) >= 1 then  m(row, col)
        else if m(row, col) == -1 then "o"
        else "·"
      }.mkString
    }
    .mkString("\n")

val m = DenseMatrix.zeros[Int](20, 40)
m(point.row, point.col) = -1

val outerRange = range+1


(0 until outerRange * 4).map{ i=>
  val y =  ((i+1) % (outerRange * 2 + 1)) - outerRange  
  val x =  if(i>(outerRange * 2 - 1)) outerRange - y.abs else y.abs - outerRange
  val row = point.row + y
  val col = point.col + x
  
  println(s"$i $y $x")
  m(row, col) = i+1
}



