package aoc2023.day7

import scala.io.Source

object Day7 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.', '/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f: String => Long): Unit =
    val solution = f(inputFile)
    val target   = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int): Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  case class Hand(cards: List[Char], withJokers: Boolean = false):
    private val (jokers, nonJokers) = cards.partition(c => withJokers && c == 'J')

    private lazy val numJokers = jokers.size

    private lazy val groups: List[Int] =
      nonJokers
        .groupBy(identity)
        .map(_._2.size)
        .toList
        .sortBy(-_) match
        case Nil  => List(numJokers)
        case largest :: other => largest + numJokers :: other


    lazy val handType: Int =
      if groups.contains(5) then 7
      else if groups.contains(4) then 6
      else if groups.contains(3) && groups.contains(2) then 5
      else if groups.contains(3) then 4
      else if groups.count(_ == 2) == 2 then 3
      else if groups.count(_ == 2) == 1 then 2
      else 1



  trait CardOrdering extends Ordering[List[Char]]:
    def cardOrder: Map[Char, Int]
    override def compare(l: List[Char], r: List[Char]): Int =
      l.zip(r).map { case (lc, rc) => cardOrder(lc) - cardOrder(rc) }.find(_ != 0).getOrElse(0)

  private object NoJokerCardOrdering extends CardOrdering:
    override val cardOrder: Map[Char, Int] = "23456789TJQKA".zipWithIndex.toMap

  private object JokerCardOrdering extends CardOrdering:
    override val cardOrder: Map[Char, Int] = "J23456789TQKA".zipWithIndex.toMap

  trait HandOrdering extends Ordering[Hand]:
    def cardOrdering: CardOrdering

    override def compare(l: Hand, r: Hand): Int =
      if l.handType == r.handType then cardOrdering.compare(l.cards, r.cards)
      else if l.handType > r.handType then 1
      else -1

  object NoJokerHandOrdering extends HandOrdering:
    override def cardOrdering: CardOrdering = NoJokerCardOrdering

  object JokerHandOrdering extends HandOrdering:
    override def cardOrdering: CardOrdering = JokerCardOrdering

  def parse(lines: Iterator[String], withJokers: Boolean) = lines.map { case s"$hand $bid" =>
    (Hand(hand.toCharArray.toList, withJokers), bid.toInt)
  }.toList

  private def solve(inputFile: String, withJokers: Boolean): Long =
    parse(readLines(inputFile), withJokers)
      .sortBy(_._1)(if withJokers then JokerHandOrdering else NoJokerHandOrdering)
      .map(_._2)
      .zipWithIndex
      .map(t => t._1 * (t._2 + 1))
      .sum

  private def solvePart1(inputFile: String): Long =
    solve(inputFile, false)

  private def solvePart2(inputFile: String): Long =
    solve(inputFile, true)

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")

  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
