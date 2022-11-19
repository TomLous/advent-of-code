package aoc2021.day2

import aoc2021.day2.model.*
import zio.{Chunk, ZIO}
import zio.stream.ZStream

object Solution {

  def followDiveInstructions(valueStream: ZStream[Any, Throwable, DiveInstruction]): ZIO[Any, Throwable, Position] = valueStream.runFold(Position(0, 0)) {
    (position, instruction) =>
      position.move(instruction)
  }

}
