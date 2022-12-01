package aoc2022.day1

import model.*
import zio.*
import zio.stream.{ZPipeline, ZStream}

object Solution {

  val stringToLongListPipeline: ZPipeline[Any, Nothing, String, List[Long]] =
    ZPipeline.splitOnChunk(Chunk("")) >>> ZPipeline.mapChunks[String, List[Long]](chunk => Chunk(chunk.toList.map(_.toLong)))

  def parseInput(stringStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[List[Long]]] =
    (for {
      list <- stringStream
        .rechunk(1)
        .via(stringToLongListPipeline)
    } yield list).runCollect.map(_.toList)

  def solvePart1(input: List[List[Long]]): ZIO[Any, Throwable, Long] =

    ZIO.succeed(input.map(_.sum).max)

  def solvePart2(input: List[List[Long]]): ZIO[Any, Throwable, Long] =
    val top3 = input.map(_.sum).sorted.takeRight(3).sum
    ZIO.succeed(top3)

}
