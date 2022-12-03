package aoc2022.day3

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input = line.toCharArray.toList.splitAt(line.length / 2)

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] = lineStream.map(parseLine).runCollect.map(_.toList)
  def parseInput2(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Char]] = (for {
    list <- lineStream
      .map(_.toCharArray.toList)
      .rechunk(3)
      .mapChunks(chunk =>
        chunk.toList match
          case a :: b :: c :: Nil => Chunk(a.toSet.intersect(b.toSet).intersect(c.toSet).head)
      )
  } yield list).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.flatMap { case (left, right) =>
      left.toSet.intersect(right.toSet).map(toNum)
    }.sum)

  def toNum(c: Char): Int =
    val i = c.toInt
    if i < 97 then i - 38 else i - 96

  def solvePart2(input: List[Char]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.map(toNum).sum)

}
