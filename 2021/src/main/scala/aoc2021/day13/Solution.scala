package aoc2021.day13

import aoc2021.day13.model.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object Solution {
  
  def parseLine(line: String): Option[Point | FoldInstruction] = line match
    case s"$x,$y" => Some(Point(x.toInt, y.toInt))
    case s"fold along $axis=$num" => Some(FoldInstruction(axis.head, num.toInt))
    case _ => None

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Origami] =
    for {
      items <- lineStream
        .map(parseLine)
        .rechunk(1)
        .runCollect
        .map(_.flatten.toList)
      origami = Origami(items.collect { case p: Point => p }, items.collect { case f: FoldInstruction => f })
    } yield origami


  def solvePart1(input: Origami): ZIO[Any, Throwable, Long] =
    val res = input.followInstructions

    println(res)
    ZIO.succeed(res.count)

  def solvePart2(input: Origami): ZIO[Any, Throwable, Long] =
    ZIO.succeed(0L)

}
