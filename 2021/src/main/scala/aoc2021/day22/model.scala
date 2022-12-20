package aoc2021.day22

import breeze.linalg.*
import breeze.numerics.*
import com.sun.org.apache.xml.internal.security.algorithms.Algorithm
import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq

object model {

  case class Cuboid(x: Range, y: Range, z: Range):
    lazy val numCubes: Long =
      x.size.toLong * y.size.toLong * z.size.toLong

    lazy val isEmpty: Boolean =
      x.isEmpty || y.isEmpty || z.isEmpty

    def intersect(other: Cuboid): Cuboid =
      Cuboid(intersectRange(x, other.x), intersectRange(y, other.y), intersectRange(z, other.z))

    def intersectRange(a: Range, b: Range): Range =
      if (a.end < b.start || a.start > b.end) Range(0, 0)
      else Range.inclusive(a.start max b.start, a.end min b.end)

  object RebootStep:
    def apply(on: Boolean, x: Range, y: Range, z: Range):RebootStep =
        RebootStep(on, Cuboid(x,y,z))

  case class RebootStep(on: Boolean, cuboid: Cuboid):

    def stepInRange(range: Range): RebootStep =
      RebootStep(on, cuboid.intersect(Cuboid(range, range, range)))




  case class Reactor(rebootSteps: List[RebootStep]):

    def cubesOnInRange(range: Range):Long =
      val filteredSteps =  rebootSteps.map(_.stepInRange(range))
      val (sum, _) = filteredSteps.foldLeft((0L, List.empty[RebootStep])) {
        case ((totalCubesOn, oldSteps), currentStep) =>
         ???




      }
      sum

}
