package aoc2021.day18

import aoc2021.day18.model.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {



  def parseLine(line: String): SnailFishNumber = SnailFishNumber(line)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[SnailFishNumber]] =
    for {
      num <- lineStream.map(parseLine).runCollect
    } yield num.toList


  def solvePart1(input: List[SnailFishNumber]): ZIO[Any, Throwable, Long] =

    val sum = input(0) + input(1)

    println(sum)

    ZIO.succeed(0L)

  def solvePart2(input: List[SnailFishNumber]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(0L)

}
