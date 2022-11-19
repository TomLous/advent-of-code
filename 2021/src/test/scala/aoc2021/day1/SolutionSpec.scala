package aoc2021.day1

import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  private val sampleList = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  def spec: Spec[Any, Throwable] = suite("Day1 Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleList)
      val expectedOutput = 7

      for {
        output <- Solution.increasedFromPrevious(input).runSum
      } yield assertTrue(output == expectedOutput)

    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleList)
      val expectedOutput = 5

      for {
        output <- Solution.increasedSumFromPreviousWindow(input).runSum
      } yield assertTrue(output == expectedOutput)

    }
  )
}
