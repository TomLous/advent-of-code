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
    private lazy val end   = graph.nodes.find(_.value.isEnd).get

    private def shortestPathLength(a: graph.NodeT, b: graph.NodeT): Long =
      graph
        .get(a)
        .shortestPathTo(graph.get(b))(Visitor.empty)
        .map(_.nodes.size - 1)
        .getOrElse(Int.MaxValue)
        .toLong

    lazy val getShortestPathSize: Long =
      shortestPathLength(start, end)

    lazy val getAnyAShortestPathSize: Long =
      graph.nodes
        .filter(_.value.weight == 'a')
        .map { startNode =>
          shortestPathLength(startNode, end)
        }
        .min

  object Hill:
    def apply(edges: Set[DiEdge[Point]]): Hill =
      Hill(Graph.from(Nil, edges))
}
