package aoc2021.day7

import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  private val sampleData: String =
    """16,1,2,0,4,2,7,1,2,14""".stripMargin

  def spec: Spec[Any, Throwable] = suite("Day7 Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 37L

      for {
        crabPositions   <- Solution.parseInput(input)
        output1       <- Solution.solvePart1(crabPositions)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 168L

      for {
        crabPositions   <- Solution.parseInput(input)
        output1       <- Solution.solvePart2(crabPositions)
      } yield assertTrue(output1 == expectedOutput)
    },

  )
}
