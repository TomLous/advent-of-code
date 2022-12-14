package aoc2021.day15

import aoc2021.day15.model.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {

  def getEdges(twoRows:List[(List[Int], Int)]):List[UnDiEdge[Cave]] = {
    twoRows match {
      case (currentRow, y) :: (nextRow, yd) :: Nil =>
        currentRow.zipWithIndex.sliding(2).toList.flatMap { case (value, x) :: (nextVal, xr) :: Nil =>
          val currentPoint = Cave(value, x, y)
          val pointR       = Cave(nextVal, xr, y)
          val pointD       = Cave(nextRow(x), x, yd)
          val pointRD      = Cave(nextRow(xr), xr, yd)

          List(
            currentPoint ~ pointR,
            currentPoint ~ pointD,
            pointR ~ pointRD,
            pointD ~ pointRD
          )
        }
    }
  }

  def expandGrid(grid:List[List[Int]]):List[List[Int]] = {
    val matrix = DenseMatrix(grid: _*)
    val factor = 5

    def increaseF(step: Int) = UFunc[Int, Int](i => (i + step - 1) % 9 + 1)

    val bigMatrix = DenseMatrix.zeros[Int](matrix.rows * factor, matrix.cols * factor)

    val mapping = for{
      i <- 0 until factor
      j <- 0 until factor
      startRow = i * matrix.rows
      endRow = startRow + matrix.rows
      startCol = j * matrix.cols
      endCol = startCol + matrix.cols
      step = i + j
    } yield (startRow, endRow, startCol, endCol, step)

    mapping.foreach { case (startRow, endRow, startCol, endCol, step) =>
      bigMatrix(startRow until endRow, startCol until endCol) := increaseF(step)(matrix)
    }

    bigMatrix.toArray.toList.grouped(bigMatrix.cols).toList
  }


  def parseLine(line: String): List[Int] = line.toCharArray.map(_.asDigit).toList

  def parseInput(lineStream: ZStream[Any, Throwable, String], expand: Boolean=false): ZIO[Any, Throwable, CaveSystem] =
    for {
      grid <- lineStream.map(parseLine).runCollect.map(_.toList)
      newGrid = if (expand) expandGrid(grid) else grid
      edges = newGrid
        .zipWithIndex
        .sliding(2)
        .flatMap(getEdges)
        .toSet
      caveSystem <- ZIO.succeed(CaveSystem(edges))
    } yield caveSystem


  def solvePart1(input: CaveSystem): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.findShortestPathScore)

  def solvePart2(input: CaveSystem): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.findShortestPathScore)

}
