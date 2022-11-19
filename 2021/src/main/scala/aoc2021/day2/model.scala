package aoc2021.day2
import scala.util.Try

object model {
  case class DiveInstruction (direction:Direction, distance:Int)
  
  object DiveInstruction:
    def fromString(s:String):Either[Throwable, DiveInstruction] = Try{
      val parts =  s.split(" ")
      DiveInstruction(Direction.valueOf(parts(0).capitalize), parts(1).toInt)
    }.toEither

  case class Position (x:Int, y:Int):
    def move(diveInstruction:DiveInstruction):Position = diveInstruction match {
      case DiveInstruction(direction, distance) => Position(direction.horizontal * distance + x, direction.vertical * distance + y)
    }

    lazy val mult:Int = x * y

  enum Direction(val horizontal:Int, val vertical: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Forward extends Direction(1, 0)

}
