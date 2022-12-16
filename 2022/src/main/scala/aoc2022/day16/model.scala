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

  case class RouteState(valve: Valve, timeSpent: Int = 0, pressure: Long = 0L, valvesToOpen: Set[Valve] = Set.empty, valvesOpenened: Set[Valve] = Set.empty):
    override def toString: String = "t:" + timeSpent + " p: " + pressure + "  r:" + valvesOpenened.map(_.name).toList.sorted.mkString(",")

  case class Volcano(graph: Graph[Valve, UnDiEdge], rootValve: Valve):
    private lazy val root: graph.NodeT = node(rootValve)

    private lazy val nodesToOpen  = graph.nodes.filter(_.value.flowRate > 0).toList
    private lazy val valvesToOpen = nodesToOpen.map(_.value).toSet
    private lazy val maxFlowRate  = valvesToOpen.map(_.flowRate).max
    private lazy val distanceToNodesWithoutRoot = nodesToOpen
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

    private lazy val distanceToNodes: Map[Valve, Map[Valve, Int]] = if (!nodesToOpen.contains(root)) {
      distanceToNodesWithoutRoot +
      (root.value -> nodesToOpen.map(to => to.value -> distance(root, to)).toMap)
    } else {
      distanceToNodesWithoutRoot
    }

    private def node(valve: Valve): graph.NodeT = graph.get(valve)

    private def distance(from: graph.NodeT, to: graph.NodeT): Int =
      from.shortestPathTo(to)(Visitor.empty).map(_.nodes.size - 1).get

    // dedupe same route states based on max pressure
    private def optimized(states: Set[RouteState]): Set[RouteState] =
      states
        .groupBy(_.valvesOpenened)
        .map { case (_, v) => v.maxBy(_.pressure) }
        .toSet

    def duoRoutes(minutes: Int): Long =
      val allRouteStates = releasePressureMaxRoutes(minutes, onlyOptimized=false)._2

      // look for all routes combo's that don't overlap, get the highest pressure combo's
      allRouteStates
        .foldLeft(0L, allRouteStates) { case ((maxCombo, statesToCheck), routeState1) =>
          val reducedSet = statesToCheck - routeState1
          val maxState1 = reducedSet
            .flatMap {
              case routeState2 if routeState1.valvesOpenened.intersect(routeState2.valvesOpenened).isEmpty =>
                Some(routeState1.pressure + routeState2.pressure)
              case _ =>
                None
            }
            .maxOption
            .getOrElse(0L)
          (maxCombo max maxState1, reducedSet)
        }
        ._1

    def releasePressureMaxRoutes(minutes: Int, onlyOptimized: Boolean=true): (Long, Set[RouteState]) =
      @tailrec
      def optimalRouteScore(states: Set[RouteState], maxPressure: Long = 0, allStates: Set[RouteState] = Set.empty): (Long, Set[RouteState]) = {
        val (newMax, newStates) = states
          .foldLeft((maxPressure, Set.empty[RouteState])) {
            // if the route will never be able to reach the max pressure, don't bother (for part 2: do bother)
            case ((currentMaxPressure, otherStates), RouteState(_, timeSpent, pressure, _, _)) if onlyOptimized && pressure + ((minutes - timeSpent) * maxFlowRate) < currentMaxPressure =>
              (currentMaxPressure, otherStates)

            // if the route has valves to explore
            case ((currentMaxPressure, otherStates), RouteState(current, timeSpent, pressure, valvesToOpen, valvesOpenened)) if valvesToOpen.nonEmpty =>
              val newStates: Set[RouteState] = valvesToOpen
                .map(next => (next, distanceToNodes(current)(next) + timeSpent + 1))  // map with distance to next valve
                .filter(_._2 <= minutes) // ignore valves that can't be reached in time
                .map { case (next, newTime) => // add the route to the new states
                  RouteState(next, newTime, pressure + next.flowRate * (minutes - newTime), valvesToOpen - next, valvesOpenened + next)
                }
              (currentMaxPressure max pressure, otherStates ++ newStates)

            // if the route has no valves to explore
            case ((currentMaxPressure, otherStates), RouteState(_, _, pressure, _, _)) =>
              (currentMaxPressure max pressure, otherStates)
          }

        // if no more states to explore, return the max pressure
        if (newStates.isEmpty)
          (newMax, allStates)
        else
          optimalRouteScore(optimized(newStates), newMax, optimized(allStates ++ newStates))
      }

      optimalRouteScore(Set(RouteState(rootValve, valvesToOpen = valvesToOpen)))

  object Volcano:
    def apply(input: List[(Valve, List[String])]): Volcano =
      val allEdges = input.foldLeft(List.empty[UnDiEdge[Valve]]) {
        case (edges, (valve, tunnels)) =>
          edges ++ tunnels.flatMap { name =>
            input.find(_._1.name == name).map(_._1 ~ valve)
          }
      }

      val root = input.find(_._1.name == "AA").get._1

      Volcano(Graph.from(Nil, allEdges), root)

}
