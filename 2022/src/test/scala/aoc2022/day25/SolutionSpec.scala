package aoc2022.day25

import aoc2022.day25.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {
  
  private val sampleData1: String =
    """1=-0-2
      |12111
      |2=0=
      |21
      |2=01
      |111
      |20012
      |112
      |1=-1=
      |1-12
      |12
      |1=
      |122""".stripMargin

  def spec: Spec[Any, Throwable] = suite("AoC 2022 - Day 25 Solution")(
    test("AoC 2022 - Day 25 - part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: String = "2=-1=0"

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    }
  )
}
