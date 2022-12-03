package aoc2021.day9

object model {

  type Input = LowPoint 
  
  case class LowPoint(value: Int, x: Long, y: Long)
}
