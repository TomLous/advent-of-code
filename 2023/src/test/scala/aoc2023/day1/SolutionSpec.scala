package aoc2023.day1

import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  private val sampleData1: String =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet
      |""".stripMargin

  def spec: Spec[Any, Throwable] = suite("AoC 2023 - Day 1 Solution")(
    test("AoC 2023 - Day 1 - part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 142

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("AoC 2023 - Day 1 - part 2 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 0

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
