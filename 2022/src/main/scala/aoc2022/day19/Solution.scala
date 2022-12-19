package aoc2022.day19

import model.*
import zio.*
import zio.stream.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq

object Solution {


  def parseLine(line: String): Blueprint = line match
    case s"Blueprint $blueprint: Each ore robot costs $oo ore. Each clay robot costs $co ore. Each obsidian robot costs $obo ore and $obc clay. Each geode robot costs $go ore and $gob obsidian." =>
      Blueprint(blueprint.toInt, OreRobot(oo.toInt), ClayRobot(co.toInt), ObsidianRobot(obo.toInt, obc.toInt), GeodeRobot(go.toInt, gob.toInt))

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Blueprint]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[Blueprint]): ZIO[Any, Throwable, Long] =
    val output = input.par.map(_.run(24))
    ZIO.succeed(output.map(_.score).sum)

  def solvePart2(input: List[Blueprint]): ZIO[Any, Throwable, Long] =
    val output = input.slice(0,3).par.map(_.run(32))
    ZIO.succeed(output.map(_.mineralState.geodeDelved).product)

}
