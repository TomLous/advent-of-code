package aoc2021.day6

import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  private val sampleData: String =
    """3,4,3,1,2""".stripMargin

  def spec: Spec[Any, Throwable] = suite("Day6 Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutputA = 26L
      val expectedOutputB = 5934L

      for {
        lanternFish   <- Solution.parseInput(input)
        output1a       <- Solution.solvePart1(lanternFish, 18)
        output1b       <- Solution.solvePart1(lanternFish, 80)
      } yield assertTrue(output1a == expectedOutputA) && assertTrue(output1b == expectedOutputB)
    },
    test("part 2 - example 1") {
      val input           = ZStream.fromIterable(sampleData.split("\n"))
      val expectedOutputA = 26984457539L


      for {
        lanternFish <- Solution.parseInput(input)
        output1a    <- Solution.solvePart1(lanternFish, 256)
      } yield assertTrue(output1a == expectedOutputA)
    }
//    test("part 1 - example 2") {
//    test("part 1 - example 2") {
//      val input          = ZStream.fromIterable(sampleData.split("\n"))
//      val expectedOutput = 12
//
//      for {
//        lineSegments <- Solution.parseInput(input)
//        output2      <- Solution.solvePart2(lineSegments)
//      } yield assertTrue(output2 == expectedOutput)
//    }
  )
}
