package aoc2021.day14


import scala.collection.immutable.HashSet
import scala.collection.parallel.CollectionConverters._

object model {


  object PolymerState:
    def apply(chars: List[Char], rules: Map[String, Char]): PolymerState =
      val pairs = chars
        .sliding(2)
        .map(_.mkString)
        .toList
        .groupMapReduce(identity)(_ => 1L)(_ + _)
       PolymerState(pairs, rules)

  case class PolymerState(pairs: Map[String, Long], rules: Map[String, Char]):



    def score:Long =
      val charSums = pairs.toList.flatMap{
        case (pair, count) => pair.toCharArray.map((_, count))
      }.groupMapReduce(_._1)(_._2)(_ + _).toList.sortBy(_._2)
      println(charSums)
      charSums.last._2 - charSums.head._2



    def iterate(n: Int) = (0 until n).foldLeft(this)((state, i) =>
      println(s"iteration ${i+1}")
      if(i == 0)
        println("NCNBCHB".toCharArray.sliding(2).map(_.mkString).map(_ -> 1L).toList.groupMapReduce(_._1)(_._2)(_ + _).toList.sortBy(_._1).toMap)
      if(i == 1)
        println("NBCCNBBBCBHCB".toCharArray.sliding(2).map(_.mkString).map(_ -> 1L).toList.groupMapReduce(_._1)(_._2)(_ + _).toList.sortBy(_._1).toMap)
      if(i == 2)
        println("NBBBCNCCNBBNBNBBCHBHHBCHB".toCharArray.sliding(2).map(_.mkString).map(_ -> 1L).toList.groupMapReduce(_._1)(_._2)(_ + _).toList.sortBy(_._1).toMap)


      state.iterateOnce
    )


    def iterateOnce: PolymerState =
      val x = pairs.toList.flatMap {
        case (pair, currentCount) =>
          rules.get(pair) match
            case Some(insertChar) =>
              List(
                (s"${pair.head}$insertChar", currentCount),
                (s"$insertChar${pair.last}", currentCount)
              )
            case None => List((pair, currentCount))
      }


//      println(x.sortBy(_._1))
//
//
      println(x.map(_._2).sum + 1)

      val y = x.groupMapReduce(_._1)(_._2)(_ + _)

      println(y)

      PolymerState(y, rules)

}
