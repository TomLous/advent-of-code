package aoc2021.day2
import aoc2021.day2.model.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Throwable] = suite("Day2 Solution")(
    test("part 1 - example 1") {
      val input = ZStream.fromIterable(
        List(
          DiveInstruction(Direction.Forward, 5),
          DiveInstruction(Direction.Down, 5),
          DiveInstruction(Direction.Forward, 8),
          DiveInstruction(Direction.Up, 3),
          DiveInstruction(Direction.Down, 8),
          DiveInstruction(Direction.Forward, 2)
        )
      )
      val expectedOutput = 150

      for {
        output <- Solution.followDiveInstructions(input)
      } yield assertTrue(output.mult == expectedOutput)

    }
  )
}
