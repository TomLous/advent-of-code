package aoc2021.day4

import aoc2021.day4.model.*
import zio.*
import zio.stream.ZStream

object Solution {

  private def parseNumbers(numberLine: String, separator:String): List[Int] =
    numberLine.trim.split(separator).map(_.trim.toInt).toList

  private def parseBoards(boardLines: List[String]): List[Board] = {


    def interpretLine(lines: List[String], grid: List[List[Int]] = Nil, boards: List[Board] = Nil): List[Board] =
      if (lines.isEmpty) boards
      else {
        val line = lines.head
        val (newGrid, newBoards) = if(line.isEmpty && grid.isEmpty )
          (Nil, boards)
        else if(line.isEmpty)
          (Nil, boards :+ Board(grid))
        else
          (grid :+ parseNumbers(line, " +"), boards)

        interpretLine(lines.tail, newGrid, newBoards)
      }

    interpretLine(boardLines :+ "")
  }

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Result] = lineStream
    .runCollect.map(_.toList match
      case numberLine :: boards =>
        Result(parseNumbers(numberLine, ","), parseBoards(boards))
  )

  def solvePart1(numbers: List[Int], boards:List[Board]): ZIO[Any, Throwable, Result] = {
    def findBoard(numbers: List[Int], boards:List[Board]): Result = {
      if numbers.isEmpty then Result()
      else{
        val number = numbers.head
        val newBoards = boards.map(_.update(number))
        val bingoes = newBoards.filter(_.bingo)
        if bingoes.nonEmpty then Result(Some(number), bingoes)
        else findBoard(numbers.tail, newBoards)
      }
    }

    ZIO.succeed(findBoard(numbers, boards))
  }

  def solvePart2(numbers: List[Int], boards:List[Board]): ZIO[Any, Throwable, Result] = {
    def findBoard(numbers: List[Int], boards:List[Board]): Result = {
      if numbers.isEmpty then Result()
      else{
        val number = numbers.head
        val newBoards = boards.map(_.update(number))
        val noBingoes = newBoards.filter(!_.bingo)
        if noBingoes.isEmpty then Result(Some(number), newBoards.headOption.toList)
        else findBoard(numbers.tail, noBingoes)
      }
    }

    ZIO.succeed(findBoard(numbers, boards))
  }
}
