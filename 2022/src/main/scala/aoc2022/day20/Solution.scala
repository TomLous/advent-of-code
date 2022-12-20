package aoc2022.day20

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): BigInt = BigInt(line.toInt)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[BigInt]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[BigInt]): ZIO[Any, Throwable, BigInt] =
    val decrypt = Decrypt(input)
    val output = decrypt.groveCoordinates(List(1000,2000,3000))
    ZIO.succeed(output.sum)

  def solvePart2(input: List[BigInt]): ZIO[Any, Throwable, BigInt] =
    val decrypt = Decrypt(input, 811589153, 10)
    val output = decrypt.groveCoordinates(List(1000,2000,3000))
    ZIO.succeed(output.sum)

}
