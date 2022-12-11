package aoc2021.day21

import aoc2021.day21.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2021 - Day 21"

  private val sampleData1: String =
    """Player 1 starting position: 4
      |Player 2 starting position: 8""".stripMargin


  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput = 739785L

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },

    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput = 444356092776315L

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
