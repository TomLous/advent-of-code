package aoc2022.day24

import aoc2022.day24.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {
  
  private val sampleData1: String =
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#""".stripMargin

  def spec: Spec[Any, Throwable] = suite("AoC 2022 - Day 24 Solution")(
    test("AoC 2022 - Day 24 - part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 18

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("AoC 2022 - Day 24 - part 2 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 0

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
