package aoc2021.day16

import aoc2021.day16.model.*
import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.hasSameElements

object SolutionSpec extends ZIOSpecDefault {

  val examples: Map[(String, (Long, Long)), Packet] = Map(
    "D2FE28"                         -> (6L, 2021L) -> Literal(2021, 6),
    "38006F4529120000"               -> (9L, 1L)    -> LTOp(1, List(Literal(10, 6), Literal(20, 2))),
    "EE00D40C823060"                 -> (14L, 3L)   -> MaxOp(7, List(Literal(1, 2), Literal(2, 4), Literal(3, 1))),
    "8A004A801A8002F478"             -> (16L, 15L)  -> MinOp(4, List(MinOp(1, List(MinOp(5, List(Literal(15, 6))))))),
    "620080001611562C8802118E34"     -> (12L, 46L)  -> SumOp(3, List(SumOp(0, List(Literal(10, 0), Literal(11, 5))), SumOp(1, List(Literal(12, 0), Literal(13, 3))))), // sum 12
    "C0015000016115A2E0802F182340"   -> (23L, 46L) -> SumOp(6, List(SumOp(0, List(Literal(10, 0), Literal(11, 6))), SumOp(4, List(Literal(12, 7), Literal(13, 0))))), // sum 23
    "A0016C880162017C3686B18A3D4780" -> (31L, 54L) -> SumOp(
      5,
      List(SumOp(1, List(SumOp(3, List(Literal(6, 7), Literal(6, 6), Literal(12, 5), Literal(15, 2), Literal(15, 2))))))
    ), // 31
    "C200B40A82"                 -> (14L, 3L) -> SumOp(6, List(Literal(1, 6), Literal(2, 2))),
    "04005AC33890"               -> (8L, 54L) -> ProdOp(0, List(Literal(6, 5), Literal(9, 3))),
    "880086C3E88112"             -> (15L, 7L) -> MinOp(4, List(Literal(7, 5), Literal(8, 6), Literal(9, 0))),
    "CE00C43D881120"             -> (11L, 9L) -> MaxOp(6, List(Literal(7, 0), Literal(8, 5), Literal(9, 0))),
    "D8005AC2A8F0"               -> (13L, 1L) -> LTOp(6, List(Literal(5, 5), Literal(15, 2))),
    "F600BC2D8F"                 -> (19L, 0L) -> GTOp(7, List(Literal(5, 7), Literal(15, 5))),
    "9C005AC2F8F0"               -> (16L, 0L) -> EqOp(4, List(Literal(5, 5), Literal(15, 7))),
    "9C0141080250320F1802104A08" -> (20L, 1L) -> EqOp(4, List(SumOp(2, List(Literal(1, 2), Literal(3, 4))), ProdOp(6, List(Literal(2, 0), Literal(2, 2)))))
  )

  private val sampleData: String =
    examples.keys.mkString("")

  def spec: Spec[Any, Throwable] = suite("Day16 Solution")(
    test("part 1 - example 1") {
      val inputStreams = examples.keys.map { case (str, _) =>
        ZStream.fromIterable(str.toCharArray)
      }
      for {
        data      <- ZIO.foreach(inputStreams)(Solution.parseInput)
        solutions <- ZIO.foreach(data)(Solution.solvePart1)
      } yield assert(solutions.toList.sorted)(hasSameElements(examples.keys.map(_._2._1).toList.sorted))

    },
    test("part 1 - show solution") {
      val inputStreams = examples.keys.map { case (str, _) =>
        ZStream.fromIterable(str.toCharArray)
      }
      for {
        data <- ZIO.foreach(inputStreams)(Solution.parseInput)
        _    <- ZIO.foreach(data)(f => Console.printLine(f.head))
      } yield assert(data.flatten)(hasSameElements(examples.values.toList))

    },
    test("part 2 - example 1") {
      val inputStreams = examples.keys.map { case (str, _) =>
        ZStream.fromIterable(str.toCharArray)
      }
      for {
        data      <- ZIO.foreach(inputStreams)(Solution.parseInput)
        solutions <- ZIO.foreach(data)(Solution.solvePart2)
      } yield assert(solutions.toList.sorted)(hasSameElements(examples.keys.map(_._2._2).toList.sorted))
    }
  )
}
