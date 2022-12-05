package aoc2022.day5

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Option[StackRow | MoveInstruction] = line match
    case s"move $num from $stackOut to $stackIn" => Some(MoveInstruction(num.toInt, stackOut.toInt, stackIn.toInt))
    case _ =>
      val map = line.toCharArray.toList.grouped(4).map(_.mkString.trim).zipWithIndex.flatMap{
        case (s"[$c]", col) => Some(col -> Box(c.head))
        case _ => None
      }.toMap

      if map.nonEmpty then Some(StackRow(map)) else None



  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Input] = for {
      items <- lineStream
        .map(parseLine)
        .rechunk(1)
        .runCollect
        .map(_.flatten.toList)
      stackRows = items.collect{case s: StackRow => s}
      moveInstructions = items.collect{case m: MoveInstruction => m}
      game = Game(stackRows, moveInstructions)
      game2 = Game(stackRows, moveInstructions, false)
    } yield (game, game2)

  def solvePart1(input: Game): ZIO[Any, Throwable, String] =
    ZIO.succeed(input.followInstructions.output)

  def solvePart2(input: Game): ZIO[Any, Throwable, String] =
    ZIO.succeed(input.followInstructions.output)

}
