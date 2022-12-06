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

    val increase = UFunc[Int, Int](_ % 9 + 1)

    val matrix2 = increase(matrix)

    def expand(m: DenseMatrix[Int], cnt: Int): DenseMatrix[Int] =
      if (cnt == 0) m
      else
        val m2 = UFunc[Int, Int](_ % 9 + 1)(m)
        expand(m2, cnt - 1)

    println(matrix2)



//    println(matrix)
//    def expandListOnce(list:List[Int]):List[Int] = list.map(_ + 1).map{case 10 => 1; case x => x}
//
//    def expandList(cnt:Int, list:List[Int]):List[Int] = cnt match {
//      case 1 => list
//      case _ => list ++ expandList(cnt - 1, expandListOnce(list))
//    }
//
//    val x = grid.map{
//      row => expandList(5, row)
//    }
//
//    x.foreach(r => println(r.mkString("")))


    grid
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
