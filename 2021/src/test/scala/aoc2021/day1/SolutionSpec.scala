package aoc2021.day1

import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Throwable] = suite("Day1 Solution")(test("example 1") {
    val input          = ZStream.fromIterable(List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263))
    val expectedOutput = 7

    for {
      output <- Solution.increasedFromPrevious(input).runSum
    } yield assertTrue(output == expectedOutput)

  })
}
