package aoc2022.day6

import aoc2022.day6.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 6"

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val data          = ZStream.fromIterable("mjqjpqmgbljsphdztnvjfqwrcgsmlb".toCharArray)
      val expectedOutput = 7L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 2") {
      val data          = ZStream.fromIterable("bvwbjplbgvbhsrlpgdmjqwftvncz".toCharArray)
      val expectedOutput = 5L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 3") {
      val data          = ZStream.fromIterable("nppdvjthqldpwncqszvftbrmjlhg".toCharArray)
      val expectedOutput = 6L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 4") {
      val data          = ZStream.fromIterable("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toCharArray)
      val expectedOutput = 10L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 5") {
      val data          = ZStream.fromIterable("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toCharArray)
      val expectedOutput = 11L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val data          = ZStream.fromIterable("mjqjpqmgbljsphdztnvjfqwrcgsmlb".toCharArray)
      val expectedOutput = 19L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 2") {
      val data          = ZStream.fromIterable("bvwbjplbgvbhsrlpgdmjqwftvncz".toCharArray)
      val expectedOutput = 23L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 3") {
      val data          = ZStream.fromIterable("nppdvjthqldpwncqszvftbrmjlhg".toCharArray)
      val expectedOutput = 23L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 4") {
      val data          = ZStream.fromIterable("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toCharArray)
      val expectedOutput = 29L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 5") {
      val data          = ZStream.fromIterable("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toCharArray)
      val expectedOutput = 26L

      for {
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    }
  )

}
