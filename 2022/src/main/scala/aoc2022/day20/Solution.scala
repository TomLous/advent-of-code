package aoc2022.day20

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Int = line.toInt

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Int]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[Int]): ZIO[Any, Throwable, Long] =
    val decrypt = Decrypt(input)
    val output = decrypt.groveCoordinates(List(1000,2000,3000))
    ZIO.succeed(output.sum)

  def solvePart2(input: List[Int]): ZIO[Any, Throwable, Long] =
    val decrypt = Decrypt(input, 811589153L, 10)
    val output = decrypt.groveCoordinates(List(1000,2000,3000))
    ZIO.succeed(output.sum)

}
