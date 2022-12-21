package aoc2020.day5

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): (BigInt, BigInt, BigInt) = line.splitAt(7) match
    case (row, col) =>
      val r = Integer.parseInt(row.map(c => if c == 'B' then 1 else 0).mkString, 2)
      val c =  Integer.parseInt(col.map(c => if c == 'R' then 1 else 0).mkString, 2)
      val s = r * 8 + c
      (r, c, s)


  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[(BigInt, BigInt, BigInt)]] = lineStream.map(parseLine).runCollect.map(_.toList)

  def solvePart1(input: List[(BigInt, BigInt, BigInt)]): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.map(_._3).max)

  def solvePart2(input: List[(BigInt, BigInt, BigInt)]): ZIO[Any, Throwable, BigInt] =
    val (rows, cols, seats) = input.unzip3
    val mySeat = for {
      rowNum <- rows.min to rows.max
      colNum <- cols.min to cols.max
      seatNum = rowNum * 8 + colNum
      if !seats.contains(seatNum) && seats.contains(seatNum - 1) && seats.contains(seatNum + 1)
    } yield seatNum
    ZIO.succeed(mySeat.head)

}
