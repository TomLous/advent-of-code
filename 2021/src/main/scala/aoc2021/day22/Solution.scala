package aoc2021.day22

import aoc2021.day22.model.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {


  def parseLine(line: String): RebootStep = line match
    case s"$on x=$xFrom..$xTo,y=$yTrom..$yTo,z=$zFrom..$zTo" => RebootStep(on == "on", xFrom.toInt to xTo.toInt, yTrom.toInt to yTo.toInt, zFrom.toInt to zTo.toInt)


  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Reactor] =
    for {
      steps <- lineStream.map(parseLine).runCollect
    } yield Reactor(steps.toList)


  def solvePart1(input: Reactor): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.cubesOnInRange(-50 to 50))

  def solvePart2(input: Reactor): ZIO[Any, Throwable, Long] =

    ZIO.succeed(0L)

}
