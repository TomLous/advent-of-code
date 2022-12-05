package aoc2021.day14

import aoc2021.day14.model.*
import zio.*
import zio.stream.*

object Solution {
  
  def parseLine(line: String): Option[List[Char] | (String, Char)] = line match
    case s"$pair -> $char" =>
      Some((pair, char.head))
    case l if l.nonEmpty => Some(l.toCharArray.toList)
    case _ => None

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, PolymerState] =
    for {
      items <- lineStream
        .map(parseLine)
        .rechunk(1)
        .runCollect
        .map(_.flatten.toList)
      template = items.collect { case p: List[Char] => p }.head
      rules = items.collect { case p: (String, Char) => p }.toMap
      polymer = PolymerState(template, rules)
    } yield polymer


  def solvePart1(input: PolymerState): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.iterate(10).score)

  def solvePart2(input: PolymerState): ZIO[Any, Throwable, Long] =
    ZIO.succeed(input.iterate(40).score)

}
