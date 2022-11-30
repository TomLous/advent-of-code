package aoc2021.day19

import aoc2021.day19.model.*
import zio.*
import zio.stream.*

object Solution {

  val stringToScannerPipeline:ZPipeline[Any, Nothing, String, Scanner] = ZPipeline.splitOnChunk(Chunk("")) >>> ZPipeline.mapChunks[String, Scanner](chunk => Chunk(Scanner(chunk.head, chunk.tail.toList)))

  def parseInput(stringStream:  ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Scanner]] =
    (for {
      list <- stringStream
        .rechunk(1)
        .via(stringToScannerPipeline)
    } yield list).runCollect.map(_.toList)

  def createSpace(input: List[Scanner]): ZIO[Any, Throwable, Space] = {
    input.foreach(i => println(i.num + " \n" + i.beaconLineSegments.map(ls => (ls.fingerprint, ls)).mkString("\n")))
    val space = Space(Nil)
    ZIO.succeed(space)
  }

  def solvePart1(input: Space): ZIO[Any, Throwable, Long] = {
    ZIO.succeed(input.beacons.size.toLong)
  }

  def solvePart2(input: Space): ZIO[Any, Throwable, Long] =
    ZIO.succeed(0L)

}
