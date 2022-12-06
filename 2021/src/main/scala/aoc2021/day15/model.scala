package aoc2021.day15

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {

  case class Cave(weight: Int, x: Long, y: Long):
    def toInner: InnerCave = InnerCave(x, y)

  case class InnerCave(x: Long, y: Long)

  object CaveSystem:
    def apply(undirectedEdges: Set[UnDiEdge[Cave]]): CaveSystem =
      val graph = Graph.from(Nil, undirectedEdges)
      val weightedEdges = for {
        node     <- graph.nodes
        neighbor <- node.neighbors
        edge = WDiEdge(neighbor.value.toInner, node.value.toInner)(node.weight)
      } yield edge

      CaveSystem(Graph.from(Nil, weightedEdges))

  case class CaveSystem(graph: Graph[InnerCave, WDiEdge]):
    private lazy val startCave = node(InnerCave(0, 0))
    private lazy val endCave   = node(InnerCave(graph.nodes.map(_.value.x).max, graph.nodes.map(_.value.y).max))

    private def node(cave: InnerCave): graph.NodeT = graph get cave

    val findShortestPathScore: Int =
     graph
        .get(startCave)
        .shortestPathTo(graph.get(endCave))(Visitor.empty)
        .map(_.weight.toInt).getOrElse(0)

    
}
