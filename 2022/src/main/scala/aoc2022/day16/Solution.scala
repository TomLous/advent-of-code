package aoc2022.day16

import model.*
import zio.*
import zio.stream.*
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq

object Solution {

  def parseLine(line: String): (Valve, List[String]) = line match
    case s"Valve $valve has flow rate=$flowRate; tunnel$_ lead$_ to valve$_ $leadsTo" => (Valve(valve, flowRate.toLong), leadsTo.split(", ").toList)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Volcano] =
    lineStream
      .map(parseLine)
      .runCollect
      .map(_.toList)
      .map(Volcano.apply)

  def solvePart1(input: Volcano): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.releasePressureMaxRoutes(30)._1)

  def solvePart2(input: Volcano): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.duoRoutes(26))



}
