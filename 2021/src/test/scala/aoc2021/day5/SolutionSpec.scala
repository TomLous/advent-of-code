package aoc2021.day5

import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  private val sampleData: String =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin

  def spec: Spec[Any, Throwable] = suite("Day5 Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 5

      for {
        lineSegments <- Solution.parseInput(input)
        output1       <- Solution.solvePart1(lineSegments)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 1 - example 2") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 12

      for {
        lineSegments <- Solution.parseInput(input)
        output2      <- Solution.solvePart2(lineSegments)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
