package aoc2021.day2

import aoc2021.day2.model.*
import zio.*
import zio.stream.ZStream

object Solution {

  def followSimpleDiveInstructions(valueStream: ZStream[Any, Throwable, DiveInstruction]): ZIO[Any, Throwable, Position] = valueStream.runFold(Position(0, 0)) {
    (position, instruction) =>
      position.move(instruction)
  }

  def followComplexDiveInstructions(valueStream: ZStream[Any, Throwable, DiveInstruction]): ZIO[Any, Throwable, ComplexPosition] = valueStream.runFold(ComplexPosition(0, 0, 0)) {
    (position, instruction) =>
      position.move(instruction)
  }

}
