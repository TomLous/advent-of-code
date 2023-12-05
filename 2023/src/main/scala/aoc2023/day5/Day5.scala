package aoc2023.day5

import scala.collection.parallel.CollectionConverters._

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source

object Day5 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.', '/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f: String => Long): Unit =
    val solution = f(inputFile)
    val target   = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int): Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  //

  private type LongRange = NumericRange.Exclusive[Long]

  extension (lr: LongRange)
    def intersection(other: LongRange): Option[LongRange] =
      val (from, to) = (lr.start max other.start) -> (lr.end min other.end)
      if from < to then Some(from until to) else None

    def exclude(other: LongRange): List[LongRange] =
        if lr == other then Nil
        else if lr.start >= other.end || lr.end <= other.start then List(lr)
        else if lr.start < other.start && lr.end > other.end then List(lr.start until other.start, other.end until lr.end)
        else if lr.start < other.start then List(lr.start until other.start)
        else if lr.end > other.end then List(other.end until lr.end)
        else throw new Exception(s"Unexpected case: $lr")

    def delta(d: Long): LongRange = (lr.start + d) until (lr.end + d)

  private def parse(lines: Iterator[String]) = lines.toList match
    case h :: t =>
      (
        h match { case s"seeds: $nums" => nums.trim.split(" ").map(_.trim.toLong).toList },
        (t :+ "").foldLeft(Map.empty[String, (String, Map[LongRange, Long])], Option.empty[((String, String), Map[LongRange, Long])]) {
          case ((mainMap, current), line) =>
            (line, current) match
              case ("", None)                         => (mainMap, None)
              case ("", Some(currentKey, currentMap)) => (mainMap + (currentKey._1 -> (currentKey._2, currentMap)), None)
              case (s"$from-to-$to map:", _)          => (mainMap, Some((from, to), Map.empty))
              case (s"$dstS $srcS $rangeS", Some((currentKey, currentMap))) =>
                val (src, dst, range) = (srcS.trim.toLong, dstS.trim.toLong, rangeS.trim.toLong)
                val srcRange = (src until src + range) -> (dst - src)
                (mainMap, Some((currentKey, currentMap + srcRange)))
        }
      )

  private def solve(seeds: LongRange, lookup: Map[String, (String, Map[LongRange, Long])]): Long =

    @tailrec
    def follow(todo: List[(String,LongRange)], currentMin:Long=Long.MaxValue): Long =
      todo match
        case Nil => currentMin // no more ranges to follow => currentMin is the solution
        case (key, srcRange) :: t => lookup.get(key) match  // otherwise get the mapping for the current key
          // no mapping => it's the end of the chain get the min
          case None =>
            follow(t, srcRange.min min currentMin)
          // otherwise follow the mapping
          case Some((dstKey, mapping)) =>
            // get all the overlapping ranges in the map
            val mappedRanges:Map[LongRange, Long] = mapping.flatMap {
              case (key, f) => (key intersection srcRange).map(_ -> f)
            }

            // get all the remaining ranges (not found in the map)
            val remainingRanges:List[LongRange] = mappedRanges.foldLeft(List(srcRange)){
              case (ranges, (key, _)) =>
                ranges.flatMap(range => range exclude key)
            }

            // combine them with the optional delta applies
            val nextRanges:List[(String,LongRange)] = (
              mappedRanges.toList.map{
                case (key, f) => key.delta(f)
              } ++ remainingRanges).map(dstRange => dstKey -> dstRange)
            follow(t ++ nextRanges, currentMin)

    follow(List("seed" -> seeds))

  private def solvePart1(inputFile: String): Long =
    val (seeds, (lookup, _)) = parse(readLines(inputFile))
    seeds.map(s => solve(s until s + 1, lookup)).min

  private def solvePart2(inputFile: String): Long =
    val (seedsRanges, (lookup, _)) = parse(readLines(inputFile))
    seedsRanges
      .grouped(2)
      .toList
      .par
      .map { case List(from, range) =>
        solve(from until from + range, lookup)
      }
      .min

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")

  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
