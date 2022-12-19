package aoc2022.day17

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): List[Char] = line.toCharArray.toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Char]] = lineStream.map(parseLine).runCollect.map(_.toList.flatten)

  def solvePart1(input: List[Char]): ZIO[Any, Throwable, Long] =
    val tetris = Tetris(input).run(2022)
    ZIO.succeed(tetris.groundHeight)

  def solvePart2(input: List[Char]): ZIO[Any, Throwable, Long] =
//    val tetris = Tetris(input).run(1000000000000L)
    ZIO.succeed(0L)

}
