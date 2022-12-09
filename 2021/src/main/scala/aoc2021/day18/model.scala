package aoc2021.day18

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {




  trait SnailFishNumber:
    def + (that: SnailFishNumber): Pair = Pair(this, that).reduce

  case class Pair(a: SnailFishNumber, b: SnailFishNumber) extends SnailFishNumber:
    override def toString: String = s"[$a,$b]"

    lazy val reduce: Pair =
      explode


    lazy val update: Pair =
      this
//      def rec(p: SnailFishNumber, offsetIndex:Int=0): (SnailFishNumber, Int) =
//        p match
//          case Pair(a, b, l, i) =>
//            val newI = offsetIndex + i + 1
//            val newL = rec(a, offsetIndex)
//            val newR = rec(b, newL._2)
//            (Pair(newL._1, newR._1, l + 1, newI), newR._2)
//          case Num(n, l, i) =>
//            (Num(n, l +1 , i + offsetIndex + 1), i + offsetIndex + 1)
//
//
//      val newP = rec(this)
//      println(newP)
//      newP._1 match
//        case p: Pair => p
//        case _ => throw new Exception("should not happen")

    lazy val explode: Pair =
//      def rec(p: SnailFishNumber, depth:Int=0): SnailFishNumber =
//        p match
//          case Pair(Num(a), Num(b)) =>
//            val newMapping = mapping :+ ((l, i), p)
//            rec(a, newMapping) ++ rec(b, newMapping)
//          case Num(_, l, i) =>
//            mapping :+ ((l, i), p)
//
//
//      val mapping = rec(this, List.empty)
      this

//      def rec(p: SnailFishNumber, level: Int = 0, leftPos: Int = 0): SnailFishNumber = p match
//        case Pair(a, b) if level < 4 => Pair(rec(a, level + 1, leftPos), rec(b, level + 1, leftPos + 1))
//
//       // get elem level 4  => parent
//
//
//
//      rec(this)


  object Pair:
    def apply(pairList: List[SnailFishNumber]): Pair = pairList match
      case l :: r :: Nil =>
        Pair(l, r)

  case class Num(n: Int) extends SnailFishNumber:
    override def toString: String = n.toString

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
