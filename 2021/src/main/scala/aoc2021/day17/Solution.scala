package aoc2021.day17

import aoc2021.day17.model.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {



  def parseLine(line: String): Target = line match{
    case s"target area: x=$xLeft..$xRight, y=$yBottom..$yTop" => Target(xLeft.toInt, xRight.toInt, yTop.toInt, yBottom.toInt)
  }

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Ocean] =
    for {
      target <- lineStream.map(parseLine).runHead
    } yield Ocean(target.get)


  def solvePart1(input: Ocean): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.maxYforTarget)

  def solvePart2(input: Ocean): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.distinctInitSpeeds)

}
