package aoc2022.day25
import scala.annotation.tailrec

object model {

  case class SNAFU(in: String):

    lazy val dec: BigInt = in.toCharArray.reverse.zipWithIndex.map { case (c, i) =>
      SNAFU.offsets(c) * SNAFU.base.pow(i)
    }.sum

  object SNAFU:
    lazy val base: BigInt = 5
    lazy val offsets: Map[Char, Int] = Map(
      '=' -> -2,
      '-' -> -1,
      '0' -> 0,
      '1' -> 1,
      '2' -> 2
    )

    lazy val reverseOffsets: Map[BigInt, (Int, Char)] = offsets.map { case (ch, offset) =>
      (offset + base) % base -> (offset, ch)
    }

    def fromDec(in: BigInt): SNAFU =
      @tailrec
      def rec(in: BigInt, acc: String): String =
        if in == 0 then acc
        else
          val (off, c) = reverseOffsets(in % base)
          rec((in - off) / base, c + acc)

      SNAFU(rec(in, ""))

}
