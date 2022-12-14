package aoc2022.day2

import aoc2022.day2.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 2"

  private val sampleData: String =
    """A Y
      |B X
      |C Z
      |""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 15L

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 12L

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
