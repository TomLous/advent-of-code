package aoc2022.day14

import aoc2022.day14.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 14"

  private val sampleData: String =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 24L

      for {
        data    <- Solution.parseInput(input)
//        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 93L

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
