package aoc2021.day11

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor

object model {

  type Input = Point

  case class Point(value: Int, x: Long, y: Long)
}
