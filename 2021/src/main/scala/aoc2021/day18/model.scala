package aoc2021.day18

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {



  trait SnailFishNumber:
    var level: Int = 0
    var index: Int = 0
    def + (that: SnailFishNumber): Pair = Pair(this, that).update

  case class Pair(a: SnailFishNumber, b: SnailFishNumber) extends SnailFishNumber:
//    override var level: Int=0
//    override var index: Int=0
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
//      def rec(p: SnailFishNumber, mapping:List[((Int, Int), SnailFishNumber)]): List[((Int, Int), SnailFishNumber)] =
//        p match
//          case Pair(a, b, l, i) =>
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
//    override var level: Int=0
//    override var index: Int=0
    override def toString: String = n.toString

  object SnailFishNumber:
    def apply(str: String): SnailFishNumber =
      def rec(chars: List[Char], currentLevel:Int = 0, currentIndex: Int = 0): (SnailFishNumber, List[Char], Int) =
        chars match
          case Nil => (null, Nil, currentIndex)
          case '[' :: tail =>
            val left  = rec(tail, currentLevel + 1, currentIndex + 1)
            val right = rec(left._2, currentLevel + 1, left._3)
            val pair = Pair(left._1, right._1)
            pair.level = currentLevel
            pair.index = currentIndex
            (pair, right._2 , right._3)
          case ',' :: tail =>
            rec(tail, currentLevel, currentIndex + 1)
          case ']' :: tail =>
            rec(tail, currentLevel , currentIndex + 1)
          case n :: tail =>
            val num = Num(n.asDigit)
            num.level = currentLevel
            num.index = currentIndex
            (num, tail, currentIndex)

      rec(str.toCharArray.toList).head

}
