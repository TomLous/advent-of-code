package aoc2020.template

import aoc2020.template.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2020 - Day [day]"

  private val sampleData: String =
    """
      |""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput: BigInt = 0

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input                  = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput: BigInt = 0

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
