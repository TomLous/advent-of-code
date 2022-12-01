package aoc2021.day19

import aoc2021.day19.model.*
import zio.*
import zio.stream.*

object Solution {

  val stringToScannerPipeline:ZPipeline[Any, Nothing, String, Scanner] = 
    ZPipeline.splitOnChunk(Chunk("")) >>> 
      ZPipeline.mapChunks[String, Scanner](chunk => Chunk(Scanner(chunk.head, chunk.tail.toList)))

  def parseInput(stringStream:  ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Scanner]] =
    (for {
      list <- stringStream
        .rechunk(1)
        .via(stringToScannerPipeline)
    } yield list).runCollect.map(_.toList)

  def createSpace(input: List[Scanner]): ZIO[Any, Throwable, Space] = {
    ZIO.succeed(Space.init(input))
  }

  def solvePart1(input: Space): ZIO[Any, Throwable, Long] = {
//    input.beacons.toList.sortBy(_.x).foreach(println)
    ZIO.succeed(input.beacons.size.toLong)
  }

  def solvePart2(input: Space): ZIO[Any, Throwable, Long] =
    ZIO.succeed(
      input.scanners.toList
        .combinations(2)
        .map { case List(l, r) =>
          l.manhattanDistance(r)
        }
        .max
        .toLong
    )

}
