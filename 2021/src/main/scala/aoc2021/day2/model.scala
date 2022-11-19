package aoc2021.day2
import scala.util.Try

object model {
  case class DiveInstruction (direction:Direction, distance:Int)

  object DiveInstruction:
    def fromString(s:String):Either[Throwable, DiveInstruction] = Try{
      val parts =  s.split(" ")
      DiveInstruction(Direction.valueOf(parts(0).capitalize), parts(1).toInt)
    }.toEither

  enum Direction(val a:Int, val b: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Forward extends Direction(1, 0)



  case class ComplexPosition (x:Int, y:Int, aim: Int):
    lazy val mult:Int = x * y
    def move(diveInstruction:DiveInstruction):ComplexPosition = diveInstruction match {
      case DiveInstruction(Direction.Forward, distance) => ComplexPosition(distance + x, aim * distance + y, aim)
      case DiveInstruction(direction, distance) => ComplexPosition(x, y, aim + (distance * direction.b))
    }
  case class Position (x:Int, y:Int):
    lazy val mult:Int = x * y
    def move(diveInstruction:DiveInstruction):Position = diveInstruction match {
      case DiveInstruction(direction, distance) => Position(direction.a * distance + x, direction.b * distance + y)
    }




}
