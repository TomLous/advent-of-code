package aoc2019.day7

import scala.io.Source
import scala.annotation.tailrec
import aoc2019.intcode.IntCode
import aoc2019.intcode.IntCode.*


object Day7 extends App:
  private def readSource(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt")
  private def readLines(inputFile: String) = readSource(inputFile).getLines()


  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")




  private def solvePart1(inputFile: String):Long =
    val program = read(readSource(inputFile))
    (0 to 4)
      .permutations
      .map(_.foldLeft(0)((acc, i) =>
        execute(
          Input(program, List(i, acc))
        )
          .sig
          .map(_.toInt)
          .head
      )).max

  case class Amp(in: List[Long], program: Program, pointer: Long = 0)

  def amplify(amp: Amp): Output =
    execute(Input(amp.program, amp.in, Some(State(amp.pointer, rb = 0))))

  def feedbackLoop(amps: List[Amp]): Int = {

    @tailrec
    def acc(xs: List[Amp], i: Int, signals: List[Int]): Int = amplify(xs(i)) match {
      case Output(_, KILL, _, _) => signals.last
      case Output(sig, p, _, state) =>
        val next    = (i + 1) % xs.length
        val nextSig = if (sig.isEmpty) 0 else sig.head.toInt
        val nextAmp = xs(next).copy(in = xs(next).in :+ nextSig)
        val amps    = xs.updated(i, Amp(Nil, state, p)).updated(next, nextAmp)
        acc(amps, next, signals :+ nextSig)
    }

    acc(amps, 0, Nil)
  }

  private def solvePart2(inputFile: String):Long =
    val program = read(readSource(inputFile))
    (5 to 9).toList.permutations.zipWithIndex
      .map(p => p._1.map(i => Amp(if (p._2 == 0) List(i, 0) else List(i), program)))
      .map(feedbackLoop)
      .max

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


//  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
//  checkSolution(part1Solution, 1)
//  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
//  checkSolution(part2Solution, 2)
