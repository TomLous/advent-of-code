package aoc2021.day5

import scala.collection.mutable
import scala.util.Try

object model {

  case class OceanFloor(lineSegments: List[LineSegment]):
    lazy val pointMap:Map[Point, Int] = lineSegments.flatMap(_.toList).groupBy(identity).map{
      case (k, v) => k -> v.size
    }

  object OceanFloor {
    def apply(lineSegments: List[LineSegment], onlyStraightLines: Boolean): OceanFloor = {
      val consideredLineSegments = if onlyStraightLines then lineSegments.filter(_.isStraight) else lineSegments

      OceanFloor(consideredLineSegments)
    }
  }

  case class LineSegment(start: Point, end: Point) extends Iterator[Point]:
    lazy val direction: Point = start.directionTo(end)
    lazy val isStraight: Boolean = direction.x == 0 || direction.y == 0

    private[this] var currentPoint = start
    private[this] var previousPoint: Point = null

    override def hasNext: Boolean = {
      val ended =  previousPoint == end
      if(ended)
        currentPoint = start
        previousPoint = null
      !ended
    }
    override def next(): Point = {
      previousPoint = currentPoint
      currentPoint = currentPoint.add(direction)
      previousPoint
    }


  case class Point(x: Int, y: Int):
    def directionTo(p: Point): Point = Point(
      p.x.compareTo(x),
      p.y.compareTo(y)
    )

    def add(direction: Point): Point = Point(x + direction.x, y + direction.y)



}
