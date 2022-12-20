package aoc2020.day3
import scala.annotation.tailrec

object model {

  type Input = List[Int]

  case class Toboggan(forest: List[List[Int]]):
    def countTrees(right: Int, down: Int): BigInt =
      val w = forest.head.size
      @tailrec
      def loop(acc: BigInt, x: Int, y: Int): BigInt =
        if y >= forest.size then acc
        else
          val row = forest(y)(x % w)
          loop(acc + row, x + right, y + down)
      loop(0, 0, 0)
}
