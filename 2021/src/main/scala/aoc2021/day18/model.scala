package aoc2021.day18

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {

  trait SnailFishNumber:

    def magnitude: Long =
      this match {
        case Num(value)        => value
        case Pair(left, right) => 3L * left.magnitude + 2L * right.magnitude
      }
    def +(that: SnailFishNumber): SnailFishNumber = Pair(this, that).reduce

  case class Num(n: Int) extends SnailFishNumber:
    override def toString: String = n.toString

  case class Pair(left: SnailFishNumber, right: SnailFishNumber) extends SnailFishNumber:
    override def toString: String = s"[$left,$right]"

    def addToLeft(number: SnailFishNumber, toAdd: Int): SnailFishNumber =
      number match {
        case Num(value)        => Num(value + toAdd)
        case Pair(left, right) => Pair(addToLeft(left, toAdd), right)
      }

    def addToRight(number: SnailFishNumber, toAdd: Int): SnailFishNumber =
      number match {
        case Num(value)        => Num(value + toAdd)
        case Pair(left, right) => Pair(left, addToRight(right, toAdd))
      }

    def explode(number: SnailFishNumber, depth: Int = 0): (Int, SnailFishNumber, Int) =
      number match {
        case regular: Num =>
          (0, regular, 0)

        case Pair(Num(left), Num(right)) if depth >= 4 =>
          (left, Num(0), right)

        case pair @ Pair(Num(_), Num(_)) =>
          (0, pair, 0)

        case pair @ Pair(left, right) =>
          lazy val (leftAddL, newLeft, leftAddR)    = explode(left, depth + 1)
          lazy val (rightAddL, newRight, rightAddR) = explode(right, depth + 1)

          if (newLeft != left) (leftAddL, Pair(newLeft, addToLeft(right, leftAddR)), 0)
          else if (newRight != right) (0, Pair(addToRight(left, rightAddL), newRight), rightAddR)
          else (0, pair, 0)
      }

    def split(number: SnailFishNumber): SnailFishNumber =
      number match {
        case Num(value) if value >= 10 =>
          Pair(Num(value / 2), Num(value - value / 2))

        case num: Num =>
          num

        case pair @ Pair(left, right) =>
          lazy val newLeft  = split(left)
          lazy val newRight = split(right)

          if (newLeft != left) Pair(newLeft, right)
          else if (newRight != right) Pair(left, newRight)
          else pair
      }

    lazy val reduce: SnailFishNumber = reduceLoop(this)
    def reduceLoop(number: SnailFishNumber): SnailFishNumber = {
      lazy val (_, tryExplode, _) = explode(number)
      lazy val trySplit           = split(number)
      if (tryExplode != number) reduceLoop(tryExplode)
      else if (trySplit != number) reduceLoop(trySplit)
      else number
    }

  object Pair:

    def apply(pairList: List[SnailFishNumber]): Pair = pairList match
      case l :: r :: Nil =>
        Pair(l, r)

  object SnailFishNumber:
    def apply(str: String): SnailFishNumber =
      def rec(chars: List[Char]): (SnailFishNumber, List[Char]) =
        chars match
          case Nil => (null, Nil)
          case '[' :: tail =>
            val left  = rec(tail)
            val right = rec(left._2)
            (Pair(left._1, right._1), right._2)
          case ',' :: tail =>
            rec(tail)
          case ']' :: tail =>
            rec(tail)
          case n :: tail =>
            val num = Num(n.asDigit)
            (Num(n.asDigit), tail)

      rec(str.toCharArray.toList).head

}
