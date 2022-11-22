package aoc2021.day3

import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  private val sampleList = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ).map(_.toList.map(_.asDigit))

  def spec: Spec[Any, Throwable] = suite("Day3 Solution")(
    test("part 1 - example 1") {
      val input          = ZStream.fromIterable(sampleList)
      val expectedOutput = 198L

      for {
        output <- Solution.calculatePowerConsumption(input)
      } yield assertTrue(output.mult == expectedOutput)
    },
    test("part 2 - example 1") {
      val input          = ZStream.fromIterable(sampleList)
      val expectedOutput = 230L

      for {
        tree   <- Solution.createBinaryTree(input)
        output <- Solution.calculateLifeSupport(tree)
      } yield assertTrue(output.mult == expectedOutput)
    }
  )
}
