package aoc2023.day5


import scala.collection.parallel.CollectionConverters._


import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source


object Day5 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  //

  private def parse(lines: Iterator[String]) = lines.toList match
    case h :: t =>
      (h match
          case s"seeds: $nums" => nums.trim.split(" ").map(_.trim.toLong).toList
        ,
        (t :+ "").foldLeft(Map.empty[String,(String,Map[(Long, Long), Long => Long])], Option.empty[((String,String),Map[(Long, Long), Long => Long])]) { case ((mainMap, current), line) =>
          (line, current) match
            case ("", None)  => (mainMap, None)
            case ("", Some(currentKey, currentMap)) => (mainMap + (currentKey._1 -> (currentKey._2, currentMap)), None)
            case (s"$from-to-$to map:", _) => (mainMap, Some((from, to), Map.empty))
            case (s"$dstS $srcS $rangeS", Some((currentKey, currentMap))) =>
              val (src,dst, range) = (srcS.trim.toLong, dstS.trim.toLong, rangeS.trim.toLong)
              val f: Long => Long = l => l + dst-src
              val newRange = (src, src+range) -> f
              (mainMap, Some((currentKey, currentMap + newRange)))
        }
    )



  private def solve(seed: Long, lookup: Map[String,(String,Map[(Long, Long), Long => Long])]):Long =

    @tailrec
    def follow(src: Long, key: String):Long =
      lookup.get(key) match
        case None => src
        case Some((dstKey, map)) =>
          val next = map.find{
            case ((from, to), _) => from <= src && src < to
          } match
            case None => src
            case Some((_, f)) => f(src)
          follow(next, dstKey)
    follow(seed, "seed")


  private def solvePart1(inputFile: String):Long =
    val (seeds, (lookup, _))  = parse(readLines(inputFile))
    seeds.map(solve(_, lookup)).min


  private def solvePart2(inputFile: String):Long =
    val (seedsRanges, (lookup, _))  = parse(readLines(inputFile))
    seedsRanges.grouped(2).toList.par.map {
      case List(from, range) =>
        from.to(from+range).foldLeft(Long.MaxValue) {
          case (currentMin, seed) => solve(seed, lookup) min currentMin
        }
    }.min


  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
