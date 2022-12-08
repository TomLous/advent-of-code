package aoc2021.day17

import aoc2021.day17.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2021 - Day 17"

  private val sampleData1: String =
    """target area: x=20..30, y=-10..-5""".stripMargin



  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput = 45L

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },

    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput = 112L

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    },

  )
}
