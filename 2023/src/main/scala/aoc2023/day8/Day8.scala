package aoc2023.day8


import scala.io.Source
import scala.annotation.tailrec
import scalax.collection.edges.labeled.*
import scalax.collection.edges.{DiEdge, DiEdgeImplicits}
import scalax.collection.immutable.Graph
import scalax.collection.immutable.DefaultGraphImpl




object Day8 extends App:
  private def readLines(inputFile: String) = Source.fromResource(getClass.getPackageName.replace('.','/') + s"/$inputFile.txt").getLines()

  private def checkExample(inputFile: String, f:String=>Long ):Unit =
    val solution = f(inputFile)
    val target = readLines(s"$inputFile-target").mkString.toLong
    assert(solution == target, s"$solution != $target -- $inputFile failed!")

  private def checkSolution(solution: Long, part: Int):Unit =
    val target = readLines(s"result-part$part").mkString.toLong
    assert(solution == target, s"$solution != $target -- Part $part failed!")

  private object Direction:
    def fromChar(c: Char): Direction = c match
      case 'L' => Left
      case 'R' => Right
      case _ => throw new Exception(s"Invalid direction: $c")

  private enum Direction:
   case Left, Right

  private class LocationEdge(override val source: String, override val target: String, override val label: Direction) extends LDiEdge[String, Direction]:
    override def equals(obj: Any): Boolean =
      super.equals(obj) && obj.isInstanceOf[LocationEdge] && obj.asInstanceOf[LocationEdge].label == label


  private def parse(lines: Iterator[String]) = lines.toList match
    case instructions :: paths => (instructions.toCharArray.map(Direction.fromChar).toList, paths.flatMap {
      case s"$node = ($left, $right)" => List(
        new LocationEdge(node, left, Direction.Left),
        new LocationEdge(node, right, Direction.Right),
      )
      case _ => Nil
    })



  private def solve(inputFile: String, part: Int, startNodesF: String => Boolean, endNodesF: String => Boolean):Long =
    val (instructions , edges) = parse(readLines(inputFile))

    val infiniteInstructions = LazyList.continually(instructions).flatten
    val graph:Graph[String, LocationEdge] = DefaultGraphImpl.from(Nil, edges)
    val rootNodes = graph.nodes.map(_.toString()).filter(startNodesF).toList.map{(_, 0L)}

    def lcm(a: Long, b: Long): Long = if (a == 0 || b == 0) 0 else Math.abs(a * b) / gcd(a, b)

    @tailrec
    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

    @tailrec
    def followInstruction(inst: LazyList[Direction], currentNodes: List[(String, Long)], stepCounts:List[Long]=Nil): Long =
      if currentNodes.isEmpty then stepCounts.reduce(lcm)
      else inst match
        case head #:: tail =>
          val (finishedNodes, stepNodes) = currentNodes.partition(t => endNodesF(t._1))
          val nextNodes = stepNodes
            .map{
              case (currentNode, step) => (graph.get(currentNode).outgoing.find(_.label == head).get.target, step + 1)
            }
          followInstruction(tail, nextNodes, stepCounts ++ finishedNodes.map(_._2))

    followInstruction(infiniteInstructions, rootNodes)


  private def solvePart1(inputFile: String):Long =
    solve(inputFile, 1, _ == "AAA", _ == "ZZZ")

  private def solvePart2(inputFile: String):Long =
    solve(inputFile, 2, _.endsWith("A"), _.endsWith("Z"))

  private lazy val part1Solution = solvePart1("input")
  private lazy val part2Solution = solvePart2("input")


  checkExample("example-part1", solvePart1)
  checkExample("example-part2", solvePart1)
  println("part1: " + part1Solution)
  checkSolution(part1Solution, 1)
  checkExample("example-part2", solvePart2)
  println("part2: " + solvePart2("input"))
  checkSolution(part2Solution, 2)
