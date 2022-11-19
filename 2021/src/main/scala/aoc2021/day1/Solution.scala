package aoc2021.day1
import zio.Chunk
import zio.stream.ZStream

object Solution {

  def increasedFromPrevious(valueStream: ZStream[Any, Throwable, Int]): ZStream[Any, Throwable, Int] = {
    val windowSize = 2
    valueStream
      .sliding(windowSize)
      .filter(_.length == windowSize)
      .map {
        case Chunk(a, b) => if (a < b) 1 else 0
        case _           => throw new Exception("This should not happen")
      }
  }

  def increasedSumFromPreviousWindow(valueStream: ZStream[Any, Throwable, Int]): ZStream[Any, Throwable, Int] = {
    val windowSize = 3
    increasedFromPrevious(
      valueStream
        .sliding(windowSize)
        .filter(_.length == windowSize)
        .map(_.sum)
    )

  }
}
