package aoc2021.day8

import scala.util.Try

object model {

  case class DigitIndex(top: Char, rightTop: Char, rightBottom: Char,leftTop: Char, leftBottom: Char, bottom: Char, middle: Char):
    assert(List(top, rightTop, rightBottom, leftTop, leftBottom, bottom, middle).toSet.size == 7, "All digits must be unique")

    val numbers: Map[Int, Set[Char]] = Map(
      0 -> Set(top, rightTop, rightBottom, leftTop, leftBottom, bottom),
      1 -> Set(rightTop, rightBottom),
      2 -> Set(top, rightTop, middle, leftBottom, bottom),
      3 -> Set(top, rightTop, middle, rightBottom, bottom),
      4 -> Set(leftTop, middle, rightTop, rightBottom),
      5 -> Set(top, leftTop, middle, rightBottom, bottom),
      6 -> Set(top, leftTop, leftBottom, middle, rightBottom, bottom),
      7 -> Set(top, rightTop, rightBottom),
      8 -> Set(top, rightTop, rightBottom, leftTop, leftBottom, middle, bottom),
      9 -> Set(top, rightTop, rightBottom, leftTop, middle, bottom)
    )

  object DigitIndex {
    def apply(data: List[String]): DigitIndex = {
      val parsed = data.map(s => (s.length, s.toCharArray.sortWith(_ < _).toSet))
      val counts = parsed.flatMap(_._2.toList).groupBy(identity).view.mapValues(_.size).toMap


      val leftTop = counts.find(_._2 == 6).get._1 // appears exactly 6 times in all digits
      val leftBottom = counts.find(_._2 == 4).get._1 // appears exactly 4 times in all digits
      val rightBottom = counts.find(_._2 == 9).get._1 // appears exactly 9 times in all digits

      val digit1 = parsed.find(_._1 == 2).map(_._2).get // has 2 segments
      val digit7 = parsed.find(_._1 == 3).map(_._2).get // has 3 segments

      val top = digit7.diff(digit1).head // segment diff between 1 and 7
      val rightTop = (counts.filter(_._2 == 8).keySet - top).head // 2 segments have 8 chars, but not the top one

      val digit4 = parsed.find(_._1 == 4).map(_._2).get // has 4 segments
      val bottomOrMiddle  = counts.filter(_._2 == 7).keySet // 2 segments appear 7 times (bottom or middle)
      val middle = digit4.intersect(bottomOrMiddle).head // only middle appears in th 4

      val bottom = (bottomOrMiddle - middle).head // bottom is the other one

      DigitIndex(top, rightTop, rightBottom, leftTop, leftBottom, bottom, middle)
    }
  }




  

  /*
      a b c d e f g
 0 =  1 1 1 0 1 1 1
 1 =  0 0 1 0 0 1 0
 2 =  1 0 1 1 1 0 1
 3 =  1 0 1 1 0 1 1
 4 =  0 1 1 1 0 1 0
 5 =  1 1 0 1 0 1 1
 6 =  1 1 0 1 1 1 1
 7 =  1 0 1 0 0 1 0
 8 =  1 1 1 1 1 1 1
 9 =  1 1 1 1 0 1 1
      8 6 8 7 4 9 7

  0:      1:      2:      3:      4:
  aaaa    ....    aaaa    aaaa    ....
 b    c  .    c  .    c  .    c  b    c
 b    c  .    c  .    c  .    c  b    c
  ....    ....    dddd    dddd    dddd
 e    f  .    f  e    .  .    f  .    f
 e    f  .    f  e    .  .    f  .    f
  gggg    ....    gggg    gggg    ....

   5:      6:      7:      8:      9:
  aaaa    aaaa    aaaa    aaaa    aaaa
 b    .  b    .  .    c  b    c  b    c
 b    .  b    .  .    c  b    c  b    c
  dddd    dddd    ....    dddd    dddd
 .    f  e    f  .    f  e    f  .    f
 .    f  e    f  .    f  e    f  .    f
  gggg    gggg    ....    gggg    gggg
  */
  case class Digit(value: Int)
  
  object Digit {
    def apply(data: String, index: DigitIndex): Digit =
      index.numbers.find {
        case (_, chars) => chars == data.toSet
      }.map(_._1).map(Digit(_)) match {
        case      Some(digit) => digit
        case     None=>
          println(s"Unknown digit: ${data.toSet} in ${index.numbers}")
          throw new IllegalArgumentException(s"Invalid digit: $data")
      }
  }
  
  case class DigitNumber(digits: List[Digit]):
    lazy val value: Int = digits.map(_.value).mkString.toInt
  


}
