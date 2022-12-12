package aoc2022.day12

import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import zio.*
import zio.stream.*

object model {

  case class Point(height: Char, x: Long, y: Long):
    lazy val isStart: Boolean = height == 'S'
    lazy val isEnd: Boolean   = height == 'E'
    lazy val weight: Char = height match
      case 'S' => 'a'
      case 'E' => 'z'
      case _   => height

    def canReach(otherPoint: Point): Boolean =
      otherPoint.weight - weight <= 1

  case class Hill(graph: Graph[Point, DiEdge]):
    private lazy val start = graph.nodes.find(_.value.isStart).get
    private lazy val end  = graph.nodes.find(_.value.isEnd).get

    private def node(point: Point): graph.NodeT = graph get point

    lazy val getShortestPathSize: Long =
      val p = graph
        .get(start)
        .shortestPathTo(graph.get(end))(Visitor.empty)
        .map(_.nodes.size - 1)
        .getOrElse(0).toLong

      println(graph)
      println(s"Shortest path size: $p")
      println(s"startCave: $start")
      println(s"endCave: $end")

      p


  object Hill:
    def apply(edges: Set[DiEdge[Point]]): Hill =
      Hill(Graph.from(Nil, edges))
}
