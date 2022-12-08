package aoc2022.day8

import model.*
import zio.*
import zio.stream.*
import breeze.generic.UFunc
import breeze.linalg.DenseMatrix

object Solution {

  def parseLine(line: String): List[Int] = line.toCharArray.map(_.asDigit).toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Grid] =
    for {
      listlist <- lineStream.map(parseLine).runCollect.map(_.toList)
      grid <- ZIO.succeed(Grid(listlist))
    } yield grid

  def solvePart1(input: Grid): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.numVisibleFromOutside)

  def solvePart2(input: Grid): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.optimalLocationScore)

}
