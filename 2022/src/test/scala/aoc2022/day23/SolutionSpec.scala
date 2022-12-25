package aoc2022.day23

import aoc2022.day23.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 23"

  private val sampleData1: String =
    """..............
      |..............
      |.......#......
      |.....###.#....
      |...#...#.#....
      |....#...##....
      |...#.###......
      |...##.#.##....
      |....#..#......
      |..............
      |..............
      |..............""".stripMargin

  private val sampleData2: String =
      """.....
        |..##.
        |..#..
        |.....
        |..##.
        |.....""".stripMargin

  def spec: Spec[Any, Throwable] = suite("AoC 2022 - Day 23 Solution")(
    test("AoC 2022 - Day 23 - part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 110

      for {
        data    <- Solution.parseInput(input)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("AoC 2022 - Day 23 - part 1 - example 2") {
      val input                  = ZStream.fromIterable(sampleData2.split("\n"))
      val expectedOutput: BigInt = 25

      for {
        data    <- Solution.parseInput(input)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("AoC 2022 - Day 23 - part 2 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 20

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    }
  )
}
