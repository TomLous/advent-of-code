package aoc2020.day1

import aoc2020.day1.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2020 - Day 1"

  private val sampleData: String =
    """1721
      |979
      |366
      |299
      |675
      |1456""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput: BigInt = 514579

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input                  = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput: BigInt = 241861950

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
