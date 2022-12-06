package aoc2022.day6

import aoc2022.day6.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2022 - Day 6"


  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val expectedOutput = 7L

      for {
        data    <- ZIO.succeed("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 2") {
      val expectedOutput = 5L

      for {
        data    <- ZIO.succeed("bvwbjplbgvbhsrlpgdmjqwftvncz")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 3") {
      val expectedOutput = 6L

      for {
        data    <- ZIO.succeed("nppdvjthqldpwncqszvftbrmjlhg")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 1 - example 4") {
      val expectedOutput = 10L

      for {
        data    <- ZIO.succeed("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 1 - example 5") {
      val expectedOutput = 11L

      for {
        data    <- ZIO.succeed("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 2 - example 1") {
      val expectedOutput = 19L

      for {
        data    <- ZIO.succeed("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 1 - example 2") {
      val expectedOutput = 23L

      for {
        data    <- ZIO.succeed("bvwbjplbgvbhsrlpgdmjqwftvncz")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 1 - example 3") {
      val expectedOutput = 23L

      for {
        data    <- ZIO.succeed("nppdvjthqldpwncqszvftbrmjlhg")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 1 - example 4") {
      val expectedOutput = 29L

      for {
        data    <- ZIO.succeed("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    },
      test("part 1 - example 5") {
      val expectedOutput = 26L

      for {
        data    <- ZIO.succeed("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart2(data)
      } yield assertTrue(output1 == expectedOutput)
    }
    )

}
