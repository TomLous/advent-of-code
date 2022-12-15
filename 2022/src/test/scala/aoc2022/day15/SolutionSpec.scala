package aoc2022.day15

import aoc2022.day15.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 15"

  private val sampleData: String =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 26L

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data, 10)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutput = 56000011L

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data, 20)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
