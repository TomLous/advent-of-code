package aoc2022.day16
import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.*
import scalax.collection.mutable.DefaultGraphImpl

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq

object model {

  case class Valve(name: String, flowRate: Long)


  case class Volcano(graph: Graph[Valve, UnDiEdge], rootValve: Valve):
    lazy val root: graph.NodeT = node(rootValve)

    private def node(valve: Valve): graph.NodeT = graph.get(valve)

    private def distance(from: graph.NodeT, to: graph.NodeT): Int =
      from.shortestPathTo(to)(Visitor.empty).map(_.nodes.size - 1).get

    def releasePressureMax(minutes: Int): Long =
      val nodesToOpen = graph.nodes.filter(_.value.flowRate > 0).toList
      val valvesToOpen = nodesToOpen.map(_.value).toSet
      val maxFlowRate = valvesToOpen.map(_.flowRate).max
      val distanceToNodesWithoutRoot = nodesToOpen
        .combinations(2)
        .flatMap { case from :: to :: Nil =>
          val d = distance(from, to)
          List(
            from.value -> (to.value   -> d),
            to.value   -> (from.value -> d)
          )
        }
        .toList
        .groupMapReduce(_._1)(t => List(t._2))(_ ++ _)
        .map { case (k, v) => k -> v.toMap }

      val distanceToNodes: Map[Valve, Map[Valve, Int]] = if (!nodesToOpen.contains(root)) {
        distanceToNodesWithoutRoot +
        (root.value -> nodesToOpen.map(to => to.value -> distance(root, to)).toMap)
      } else {
        distanceToNodesWithoutRoot
      }

      case class RouteState(valve: Valve, timeSpent: Int = 0, pressure: Long = 0L, valvesToOpen: Set[Valve] = Set.empty, valvesOpenened: Set[Valve] = Set.empty)

      @tailrec
      def optimalRouteScore(states: Set[RouteState], maxPressure: Long = 0): Long = {
        val (newMax, newStates) = states
          .foldLeft((maxPressure, Set.empty[RouteState])) {
            case ((currentMaxPressure, otherStates), RouteState(current, timeSpent, pressure, valvesToOpen, valvesOpenened)) if valvesToOpen.nonEmpty =>
              val newStates: Set[RouteState] = valvesToOpen
                .map(next => (next, distanceToNodes(current)(next) + timeSpent + 1))
                .filter(_._2 <= minutes)
                .map { case (next, newTime) =>
                  RouteState(next, newTime, pressure + next.flowRate * (minutes - newTime), valvesToOpen - next, valvesOpenened + next)
                }.filter(route =>
                    val remaining = minutes - route.timeSpent
                    val maxPotentialPressure = route.pressure + remaining * maxFlowRate
                    maxPotentialPressure > currentMaxPressure
                )
              (currentMaxPressure max pressure, otherStates ++ newStates)
            case ((currentMaxPressure, otherStates), RouteState(_, _, pressure, _, _)) =>
              (currentMaxPressure max pressure, otherStates)
          }

        if (newStates.isEmpty)
          newMax
        else
          optimalRouteScore(newStates, newMax)
      }


      optimalRouteScore(Set(RouteState(rootValve, valvesToOpen=valvesToOpen)))



  object Volcano:
    def apply(input: List[(Valve, List[String])]): Volcano =
      val allEdges = input.foldLeft(List.empty[UnDiEdge[Valve]]) { case (edges, (valve, tunnels)) =>
        val newEdges = tunnels.map { name =>
          input.find(_._1.name == name).map(_._1 ~ valve).get
        }
        edges ++ newEdges
      }

      Volcano(Graph.from(Nil, allEdges), input.find(_._1.name == "AA").get._1)

}
