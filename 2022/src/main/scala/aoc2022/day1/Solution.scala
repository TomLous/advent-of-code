package aoc2022.day1

import aoc2022.day1.model.*
import zio.*
import zio.stream.{ZPipeline, ZStream}

object Solution {

  val stringToLongListPipeline: ZPipeline[Any, Nothing, String, Input] =
    ZPipeline.splitOnChunk(Chunk("")) >>>
    ZPipeline.mapChunks[String, List[Long]](chunk => Chunk(chunk.toList.map(_.toLong)))

  def parseInput(stringStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Input]] =
    (for {
      list <- stringStream
        .rechunk(1)
        .via(stringToLongListPipeline)
    } yield list).runCollect.map(_.toList)

  def solvePart1(input: List[Input]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.map(_.sum).max)

  def solvePart2(input: List[Input]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.map(_.sum).sorted.takeRight(3).sum)

}
