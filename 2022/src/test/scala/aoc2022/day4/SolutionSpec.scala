package aoc2022.day4

import aoc2022.day4.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 4"

  private val sampleData: String =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8
      |""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 2L

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 4L

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
