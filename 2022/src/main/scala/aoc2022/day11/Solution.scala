package aoc2022.day11

import model.*
import zio.*
import zio.stream.*

object Solution {


  def parseLine(line: String): Option[Input] =  line.trim match
    case s"Monkey $id:" => Some(InpMonkeyId(id.toInt))
    case s"Starting items: $items" => Some(InpMonkeyItems(items.split(", ").map(_.trim.toInt).toList))
    case s"Operation: new = old $op $value" => Some(InpMonkeyOp(op.head, value))
    case s"Test: divisible by $div" => Some(InpMonkeyTest(div.toInt))
    case s"If true: throw to monkey $monkeyId" => Some(InpMonkeyTestResult(true, monkeyId.toInt))
    case s"If false: throw to monkey $monkeyId" => Some(InpMonkeyTestResult(false, monkeyId.toInt))
    case _ => None

  val monkeyInputToMonkey = ZPipeline.mapChunks[Option[Input], Monkey] { chunk =>
    Chunk(Monkey(chunk.flatten.toList))
  }

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, MonkeyGroup] =
    lineStream
      .map(parseLine)
      .rechunk(7)
      .via(monkeyInputToMonkey)
      .runCollect
      .map(monkeys => MonkeyGroup(monkeys.toList))



  def solvePart1(input: MonkeyGroup): ZIO[Any, Throwable, Long] =
    val end = (0 until 20).foldLeft(input){
      case (input, _) =>
        input.round(_ / 3L)
    }
    ZIO.succeed(end.monkeys.map(_.inspectionCounter).sorted.takeRight(2).product)
  

  def solvePart2(input: MonkeyGroup, numRounds: Int=10000): ZIO[Any, Throwable, Long] =
    val end = (0 until numRounds).foldLeft(input){
      case (input, _) =>
        input.round(_ / 1L)
    }
    
    ZIO.succeed(end.monkeys.map(_.inspectionCounter).sorted.takeRight(2).product)

}
