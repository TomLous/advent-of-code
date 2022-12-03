package aoc2021.day9

import aoc2021.day9.model.*
import zio.*
import zio.stream.*

object Solution {

  val findLowestPoint = ZPipeline.mapChunks[Chunk[(List[Int], Long)], Point](_.flatMap(_.toList match
    case (previousLine, _) :: (currentLine, y) :: (nextLine, _) :: Nil =>
      Chunk.fromIterator(
        (None +: currentLine.zipWithIndex.map(Some(_)) :+ None).sliding(3).flatMap{
          case previousItem :: Some(value, pos) :: nextItem :: Nil =>
            val up = previousLine.lift(pos)
            val down = nextLine.lift(pos)
            val left = previousItem.map(_._1)
            val right = nextItem.map(_._1)
            if(List(up, down, left, right).flatten.forall(_ > value)) Some(Point(value, pos.toLong, y)) else None
        }
      )
    case other => throw new Exception(s"Unexpected list: $other")
  ))

  def parseLine(line: String): List[Int] = line.toCharArray.map(_.asDigit).toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] =
    (ZStream.fromChunk(Chunk(List.empty[Int])) ++ lineStream
      .map(parseLine) ++ ZStream.fromChunk(Chunk(List.empty[Int])))
      .zipWithIndex
      .rechunk(1)
      .sliding(3)
      .via(findLowestPoint)
      .runCollect
      .map(_.toList)


  def solvePart1(input: List[Input]): ZIO[Any, Throwable, Long] =

    ZIO.succeed(input.map(_.value + 1).sum)

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, Long] =
    // TODO: implement
    ZIO.succeed(0L)

}
