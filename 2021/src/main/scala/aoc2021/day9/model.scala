package aoc2021.day9
import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor

object model {

  type Input = Point

  case class Point(value: Int, x: Long, y: Long):
    def createEdge(other: Point): DiEdge[Point] = if value < other.value then DiEdge(other, this) else DiEdge(this, other)
}
