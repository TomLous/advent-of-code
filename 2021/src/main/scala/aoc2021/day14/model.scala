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
      val counts = chars.groupMapReduce(identity)(_ => 1L)(_ + _)
       PolymerState(pairs, counts, rules)

  case class PolymerState(pairs: Map[String, Long], charCount:Map[Char, Long], rules: Map[String, Char]):

    def score:Long =
      val charSums = charCount.toList.sortBy(_._2)
      charSums.last._2 - charSums.head._2
    def iterate(n: Int): PolymerState = (0 until n).foldLeft(this)((state, i) => state.iterateOnce)


    def iterateOnce: PolymerState =
      val (newPairsRaw, addSumsRaw) = pairs.toList.flatMap {
        case (pair, currentCount) =>
          rules.get(pair) match
            case Some(insertChar) =>
              List(
                ((s"${pair.head}$insertChar", currentCount), Some((insertChar,currentCount))),
                ((s"$insertChar${pair.last}", currentCount), None)
              )
            case None =>
              List(
                ((pair, currentCount), None)
              )
      }.unzip

      val newPairs = newPairsRaw.groupMapReduce(_._1)(_._2)(_ + _)
      val newSums = (addSumsRaw.flatten ++ charCount.toList).groupMapReduce(_._1)(_._2)(_ + _)

      PolymerState(newPairs, newSums, rules)

}
