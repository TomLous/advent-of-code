package aoc2022.day6

import model.*
import zio.*
import zio.stream.*

object Solution {
  
  def findNonRepeatingIndex(input: String, size: Int): Option[Int] =
    input
      .toCharArray
      .zipWithIndex
      .sliding(size)
      .map(a => a.last._2 -> a.map(_._1).toSet.size)
      .find(_._2 == size)
      .map(_._1)

  def solvePart1(input: String): ZIO[Any, Throwable, Long] =
    val noRepeat = findNonRepeatingIndex(input, 4)
    ZIO.succeed(noRepeat.get + 1)

  def solvePart2(input: String): ZIO[Any, Throwable, Long] =
    val noRepeat = findNonRepeatingIndex(input, 14)
    ZIO.succeed(noRepeat.get + 1)
}
