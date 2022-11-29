package aoc2021.day16

import aoc2021.day16.model.*
import zio.*
import zio.stream.*

object Solution {

  def hexToBinary(hex: Char): Seq[Int] = Integer
    .parseInt(hex.toString, 16)
    .toBinaryString
    .reverse
    .padTo(4, '0')
    .reverse
    .map(_.asDigit)

  val hexToBinaryPipeline: ZPipeline[Any, Nothing, Char, Int] =
    ZPipeline.map[Char, Chunk[Int]](hex => Chunk.fromIterable(hexToBinary(hex))) >>>
    ZPipeline.mapChunks[Chunk[Int], Int](_.flatten)

  val parseDigits: ZPipeline[Any, Nothing, Int, Packet] = ZPipeline.fromPush {
    for {
      packetChunckStateRef <- Ref.make(PacketChunkState())
    } yield { (optChunk: Option[Chunk[Int]]) =>
      optChunk match {
        case None =>
          for {
            _ <- packetChunckStateRef.set(PacketChunkState())
          } yield Chunk()
        case Some(chunk) =>
          for {
            packetChunckState <- packetChunckStateRef.get
            packetStateChunkResult = packetChunckState.processChunk(chunk)
            _ <- packetChunckStateRef.set(packetStateChunkResult.state)
          } yield packetStateChunkResult.packets
      }
    }
  }

  def parseInput(charStream: ZStream[Any, Throwable, Char]): ZIO[Any, Throwable, List[Packet]] =
    (for {
      list <- charStream
        .via(hexToBinaryPipeline)
        .rechunk(3)
        .via(parseDigits)
    } yield list).runCollect.map(_.toList)
  //    val listEffect = for {
  //      list <- charStream
  //        .via(hexToBinaryPipeline)
  //        .runCollect
  //    } yield list

//    val list = Unsafe.unsafe { implicit unsafe =>
//      Runtime.default.unsafe.run(listEffect).getOrThrowFiberFailure()
//    }
//    println(list.mkString(""))

  def solvePart1(input: List[Packet]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.map(_.versionSum).sum.toLong)

  def solvePart2(input: List[Packet]): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.map(_.value).sum)

}
