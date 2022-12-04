package aoc2021.day11

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.mutable.DefaultGraphImpl

import scala.collection.immutable.HashSet

object model {

  type Input = Point



  case class Point(x: Long, y: Long):
    override def toString: String = s"($x, $y) => $value"

    var value: Int = 0

    def plusOne: Point =
      value += 1
      this

    def reset: Point =
      value = 0
      this

    def flashed: Boolean = value > 9

    def flashedFirst: Boolean = value == 10


  object Point:
    def apply(value: Int, x: Long, y:Long): Point =
      val p = Point(x, y)
      p.value = value
      p

  object Grid:
    def apply(edges: Set[UnDiEdge[Point]]): Grid = Grid(Graph.from(Nil, edges))

  case class Grid(graph: Graph[Point, UnDiEdge]):
    var flashCounter:Int = 0
    var iterationCounter:Int = 0
    var flashSyncAt:Int = -1
    override def toString: String =
      val points:Map[(Long, Long), Int] = graph.nodes.map(_.value).toList.map{p =>
        (p.x, p.y) -> p.value
      }.toMap

      val width = points.keys.map(_._1).max.toInt
      val height = points.keys.map(_._2).max.toInt

      val s = for{
        y <- 0 to height
        x <- 0 to width
        s <- points.get((x, y)).map(_.toString).map(s => if x == 0 then s"\n$s" else s)
      } yield s

      s.mkString("")

    def iterateOnce: Unit =
      iterationCounter += 1
      graph.nodes.map(_.value.plusOne) // add 1 to all nodes
      val flashedNodes:List[graph.NodeT]= graph.nodes.filter(_.value.flashed).toList

      def traverse(nodes: List[graph.NodeT]): Unit =
        nodes.foreach(_.neighbors.foreach { n =>
          n.value.plusOne
          if(n.value.flashedFirst) traverse(List(n))
        })

      traverse(flashedNodes)

      graph.nodes.filter(_.value.flashed).foreach(node => {
        flashCounter += 1
        node.value.reset
      }) // reset all flashed nodes


    def iterate(n: Int):Grid =
      (0 until n).foreach(_ => iterateOnce)
      this

    def iterateUntilFlashSync:Grid =
      while(flashSyncAt < 0)
        iterateOnce
        if(graph.nodes.forall(_.value.value == 0))
          flashSyncAt = iterationCounter
          

      this



}
