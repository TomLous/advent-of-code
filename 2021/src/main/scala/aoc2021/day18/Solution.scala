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
    ZIO.succeed(input.reduce(_ + _).magnitude)

  def solvePart2(input: List[SnailFishNumber]): ZIO[Any, Throwable, Long] =

    val max = (for{
      combination  <- input.combinations(2)
      permutation  <- combination.permutations
      sum           =  permutation.reduce(_ + _)
    } yield sum.magnitude).max

    ZIO.succeed(max)

}
