package aoc2021.day18

import aoc2021.day18.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2021 - Day 18"

  private val sampleData1: String =
    """[1,2]
      |[[1,2],3]
      |[9,[8,7]]
      |[[1,9],[8,5]]
      |[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
      |[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
      |[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]""".stripMargin

  private val sampleData2: String =
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
      |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
      |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
      |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
      |[7,[5,[[3,8],[1,4]]]]
      |[[2,[2,2]],[8,[8,1]]]
      |[2,9]
      |[1,[[[9,3],9],[[9,0],[0,7]]]]
      |[[[5,[7,4]],7],1]
      |[[[[4,2],2],6],[8,7]]""".stripMargin



  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput = 4140L

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 2") {
      val input          = ZStream.fromIterable(sampleData2.split("\n"))
      val expectedOutput = 4140L

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
