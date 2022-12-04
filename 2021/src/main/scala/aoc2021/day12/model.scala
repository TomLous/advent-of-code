package aoc2021.day12

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.mutable.DefaultGraphImpl

import scala.collection.immutable.HashSet

object model {

  trait Cave:
    def name: String

  object Cave:
    def apply(name: String): Cave =
      if (name == "start" || name == "end") OuterCave(name)
      else if (name.head.isUpper) BigCave(name)
      else SmallCave(name)


  case class BigCave(name: String) extends Cave
  case class SmallCave(name: String) extends Cave
  case class OuterCave(name: String) extends Cave


  object CaveSystem:
    def apply(edges: Set[UnDiEdge[Cave]]): CaveSystem = CaveSystem(Graph.from(Nil, edges))


  case class CaveSystem(graph: Graph[Cave, UnDiEdge]):
    def node(cave: Cave): graph.NodeT = graph get cave

    def findPaths(allowSecondVisitOnce: Boolean = false): Set[List[Cave]] =
      val fromCave = node(OuterCave("start"))
      val toCave = node(OuterCave("end"))

      def hasVisitedTwice(path: List[Cave]): Boolean =
        path
          .collect({case s:SmallCave => s})
          .groupBy(identity)
          .values
          .map(_.size)
          .exists(_ > 1)

      def traverseCaves(node: graph.NodeT, currentPath: List[Cave]=Nil): Set[List[Cave]] =
        if(node == toCave) Set(currentPath :+ node.value)
        else
          val nextNodes = node.neighbors.filter(_.value match {
            case _:BigCave => true
            case s:SmallCave if allowSecondVisitOnce && currentPath.contains(s) =>
              !hasVisitedTwice(currentPath :+ node.value)
            case c  => !currentPath.contains(c)
          })
          nextNodes.flatMap(n => traverseCaves(n, currentPath :+ node.value)).toSet

      traverseCaves(fromCave)





}
