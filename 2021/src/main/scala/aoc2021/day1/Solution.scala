package aoc2021.day1
import zio.Chunk
import zio.stream.ZStream

object Solution {

  def increasedFromPrevious(valueStream: ZStream[Any, Throwable, Int]): ZStream[Any, Throwable, Int] = {
    valueStream
      .sliding(2)
      .filter(_.length == 2)
      .map{
        case Chunk(a, b) => if (a < b) 1 else 0
        case _ => throw new Exception("This should not happen")
      }
  }
}
