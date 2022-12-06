package aoc2022.day6

import model.*
import zio.*
import zio.stream.*

object Solution {

  def findNonRepeatingIndex(charStream: ZStream[Any, Throwable, Char], size: Int): ZIO[Any, Throwable, Long] =
    charStream
      .zipWithIndex
      .sliding(size)
      .map(a => a.last._2 -> a.map(_._1).toSet.size)
      .find(_._2 == size)
      .map(_._1)
      .runHead
      .map(_.get) // just die if it's not found


  def solvePart1(input: ZStream[Any, Throwable, Char]): ZIO[Any, Throwable, Long] =
    for {
      index <- findNonRepeatingIndex(input, 4)
    } yield index + 1

  def solvePart2(input: ZStream[Any, Throwable, Char]): ZIO[Any, Throwable, Long] =
    for {
      index <- findNonRepeatingIndex(input, 14)
    } yield index + 1
}
