package aoc2022.day22

import aoc2022.day22.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 22"

  private val sampleData1: String =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5
      |""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test(s"AoC 2022 - Day 22 - part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 6032

      for {
        data    <- Solution.parseInput(input)
//        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
//    test(s"$prefix - part 2 - example 1") {
//      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
//      val expectedOutput: BigInt = 0
//
//      for {
//        data    <- Solution.parseInput(input)
//        output2 <- Solution.solvePart2(data)
//      } yield assertTrue(output2 == expectedOutput)
//    } @@ ignore
  )
}
