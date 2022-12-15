package aoc2022.day15

import breeze.generic.UFunc
import breeze.linalg.*
import breeze.linalg.operators.*
import breeze.numerics.*

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq
import scala.util.Try

object model {

  case class Point(row: Long, col: Long):
    def manhattanDistance(other: Point): Long = math.abs(col - other.col) + math.abs(row - other.row)
    lazy val tuningFrequency: Long            = (col * 4000000L) + row
  
  case class Zone(sensorBeacons: List[(Point, Point)]):
    lazy val sensorRange: Map[Point, Long] = sensorBeacons.map { case (sensor, beacon) =>
      sensor -> sensor.manhattanDistance(beacon)
    }.toMap

    def sensorOuterPerimeter(maxRange: Int): ParSeq[Point] =
      for {
        (sensor, range) <- sensorRange.toList.par
        outerRange = range + 1 // check the outside of the range
        i <- 0L until outerRange * 4L // 4 sides of the diamond
        y   = ((i + 1) % (outerRange * 2 + 1)) - outerRange // mod 2* the range with offset of 1 fon non overlapping points so from top -> bottom 2x
        x   = if (i >= outerRange * 2) outerRange - y.abs else y.abs - outerRange // for the first loop do left side, otherside do right side
        row = sensor.row + y
        col = sensor.col + x //add to sensor position
        if col >= 0 && row >= 0 && col <= maxRange && row <= maxRange //check if within the absolute max range (0 - maxRange)
      } yield Point(row, col)

    lazy val points: List[Point] = sensorBeacons.flatMap { case (sensor, beacon) =>
      List(sensor, beacon)
    }


    lazy val (minRow, maxRow, minCol, maxCol) = sensorRange
      .foldLeft((0L, 0L, 0L, 0L)) { case ((minRow, maxRow, minCol, maxCol), (sensor, range)) =>
        val rowPos = List(
          sensor.row - range,
          sensor.row + range,
          minRow,
          maxRow
        )
        val colPos = List(
          sensor.col - range,
          sensor.col + range,
          minCol,
          maxCol
        )

        (rowPos.min, rowPos.max, colPos.min, colPos.max)
      }

    def countVoid(row: Long): Long =
      (minCol to maxCol).count { col =>
        val checkInRange = Point(row, col)
        !points.contains(checkInRange) && sensorRange.exists { case (sensor, range) =>
          sensor.manhattanDistance(checkInRange) <= range
        }
      }

    def findNondetecteableBeacon(maxRange: Long): Point =
      sensorOuterPerimeter(maxRange.toInt)
        .dropWhile(perimeterPoint =>
          sensorRange.exists { case (sensor, range) =>
            sensor.manhattanDistance(perimeterPoint) <= range
          }
        )
        .head

}
