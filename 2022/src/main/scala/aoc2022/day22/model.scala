package aoc2022.day22

import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.Implicits.*
import scalax.collection.edge.*
import breeze.linalg.*
import geometry.Pos2

import scala.annotation.tailrec

object model {

  trait Point
  case class Valid(x: BigInt, y: BigInt) extends Point
  case class Wall(x: BigInt, y: BigInt) extends Point

  enum Direction:
    case Left, Right

  enum Orientation(val pos: Pos2, val x: Int):
    case Up extends Orientation(Pos2(0, -1), 3)
    case Down extends Orientation(Pos2(0, 1), 1)
    case Left extends Orientation(Pos2(-1, 0), 2)
    case Right extends Orientation(Pos2(1, 0), 0)


  case class Command(command: Direction, steps: BigInt)


  case class GameBoard(commands: List[Command], fieldGrid: DenseMatrix[Int]):

    def matrixToString(m: DenseMatrix[Int], steps:Map[Int, (Pos2, Orientation)]): String =
      val lfind = steps.map{
        case (step, (pos, orientation)) =>
          (pos.row, pos.col) -> (step, orientation)
      }.toMap

      (0 until m.rows)
        .map { row =>
          (0 until m.cols).map { col =>
            val value = m(row, col)
            if(lfind.contains((row, col)))
              lfind((row, col))._1.toString
//              lfind((row, col))._2 match
//                case Orientation.Up => "^"
//                case Orientation.Down => "v"
//                case Orientation.Left => "<"
//                case Orientation.Right => ">"
            else if value == 2 then "█"
            else if value == 1 then "."
            else " "
          }.mkString
        }
        .mkString("\n"
        )

    def move(point: Pos2, orientation: Orientation, steps: BigInt): Pos2 =
      @tailrec
      def next(stepsRemaining: BigInt, currentPoint: Pos2):Pos2 =
        if(stepsRemaining == 0) currentPoint
        else
          val nextPoint = currentPoint + orientation.pos
          if(nextPoint.row >= fieldGrid.rows || nextPoint.col >= fieldGrid.cols || nextPoint.row < 0 || nextPoint.col < 0 || fieldGrid(nextPoint.row, nextPoint.col) == 0)
            val nextPos = orientation match {
                case Orientation.Up =>
                  fieldGrid(::, currentPoint.col).t.inner.toArray.toList.zipWithIndex.findLast(_._1 > 0).map(_._2).map(Pos2(currentPoint.col, _))
                case Orientation.Down =>
                  fieldGrid(::, currentPoint.col).t.inner.toArray.toList.zipWithIndex.find(_._1 > 0).map(_._2).map(Pos2(currentPoint.col, _))
                case Orientation.Left =>
                  fieldGrid(currentPoint.row, ::).inner.toArray.toList.zipWithIndex.findLast(_._1 > 0).map(_._2).map(Pos2(_, currentPoint.row))
                case Orientation.Right =>
                  fieldGrid(currentPoint.row, ::).inner.toArray.toList.zipWithIndex.find(_._1 > 0).map(_._2).map(Pos2(_, currentPoint.row))
            }



//            println(s"m: ${fieldGrid.rows} x ${fieldGrid.cols}")
//            println(s"orientation: $orientation")
//            println(s"currentPoint: $currentPoint")
//            println(s"nextPoint: $nextPoint")
//            println(s"np: $nextPos")
//            System.exit(1)



            val wrappedPoint = nextPos.get

            if(fieldGrid(wrappedPoint.row, wrappedPoint.col) == 2) currentPoint
            else next(stepsRemaining - 1, wrappedPoint)
          else if(fieldGrid(nextPoint.row, nextPoint.col) == 2) currentPoint // hit a wall
          else next(stepsRemaining - 1, nextPoint)



      next(steps, point)








    lazy val runPart1: (Pos2, Orientation) = {
      val startFrom = Pos2(fieldGrid(0, ::).inner.toArray.indexOf(1), 0)


      @tailrec
      def step(remainingCommands: List[Command], currentPoint: Pos2, currentOrientation: Orientation , stepcnt: Int, steps: Map[Int, (Pos2, Orientation)]): (Pos2, Orientation, Map[Int, (Pos2, Orientation)]) =  remainingCommands match
        case Nil => (currentPoint, currentOrientation, steps)
        case headCommand :: tailCommands =>
          println(s"----")
          println(s"step: $stepcnt")
          println(s"cp: $currentPoint")
          println(s"or: $currentOrientation")
          println(s"take ${headCommand.steps} then turn ${headCommand.command}")
          val newPoint = move(currentPoint, currentOrientation, headCommand.steps)
          val newOrientation = headCommand.command match
            case Direction.Left => currentOrientation match
              case Orientation.Up => Orientation.Left
              case Orientation.Down => Orientation.Right
              case Orientation.Left => Orientation.Down
              case Orientation.Right => Orientation.Up
            case Direction.Right => currentOrientation match
              case Orientation.Up => Orientation.Right
              case Orientation.Down => Orientation.Left
              case Orientation.Left => Orientation.Up
              case Orientation.Right => Orientation.Down
          println(s"newPoint: $newPoint")

          val x = steps + (stepcnt -> (newPoint, newOrientation))
//          if(stepcnt == 9)
//            println(matrixToString(fieldGrid, x))
//            throw new Exception("stop")

          step(tailCommands,  newPoint, newOrientation, stepcnt+1, x)


      val (pos, or, steps) = step(commands, startFrom, Orientation.Right, 0,  Map(0 -> (startFrom, Orientation.Right)))



      (pos, or)
    }


  object GameBoard:
    def parseCommands(str: String): List[Command] =
      val pattern = """(\d*)([LR])""".r
      pattern.findAllIn(str).matchData.map { m =>
        val steps = m.group(1).toInt
        val command = m.group(2) match
          case "L" => Direction.Left
          case "R" => Direction.Right
        Command(command, steps)
      }.toList


    def apply(input: List[String]): GameBoard =
      val commands = parseCommands(input.last)

      val field = input.dropRight(2)
      val maxWidth = field.map(_.length).max
      val field2 = field.map(_.padTo(maxWidth, ' ').toCharArray.map {
        case ' ' => 0
        case '#' => 2
        case '.' => 1
      })

      val m = DenseMatrix(field2: _*)

//      field2.foreach(println)
//      println(m)

      GameBoard(commands, m)


//      val points = input.flatMap(_._1)
//      val edges = input.flatMap(_._2)
//      val graph = Graph.from(points, edges)

//       def getPoint(char: Char, x: BigInt, y: BigInt): Option[Point] =
//    if (char == '.') Some(Valid(x, y))
//    else if (char == '#') Some(Wall(x, y))
//    else None
//
//
//
//  val getEdges = ZPipeline.mapChunks[Chunk[(List[Char], Long)], (List[Point], List[LDiEdge[Point]])](_.flatMap(_.toList match
//    case (currentRow, y) :: (nextRow, yd) :: Nil =>
//      currentRow.zipWithIndex.sliding(2).toList.flatMap { case (ch, x) :: (chNext, xr) :: Nil =>
//        println(currentRow)
//        println(nextRow)
//
//        val currentPoint = getPoint(ch, x, y)
//        val pointR       = getPoint(chNext, xr, y)
//        val pointD       = getPoint(nextRow(x), x, yd)
//        val pointRD      = getPoint(nextRow(xr), xr, yd)
//
//        val edges: List[LDiEdge[Point]] = List(
//          (currentPoint, pointR) match {
//            case (Some(cp: Valid ), Some(pr: Valid)) => List(
//              (cp ~+> pr) (Direction.Right),
//              (pr ~+> cp) (Direction.Left)
//            )
//            case _ => Nil
//          },
//          (currentPoint, pointD) match {
//            case (Some(cp: Valid ), Some(pd: Valid)) => List(
//              (cp ~+> pd) (Direction.Down),
//              (pd ~+> cp) (Direction.Up)
//            )
//            case _ => Nil
//          },
//          (pointR, pointRD) match {
//            case (Some(pr: Valid ), Some(prd: Valid)) => List(
//              (pr ~+> prd) (Direction.Down),
//              (prd ~+> pr) (Direction.Up)
//            )
//            case _ => Nil
//          },
//          (pointD, pointRD) match {
//            case (Some(pd: Valid ), Some(prd: Valid)) => List(
//              (pd ~+> prd) (Direction.Right),
//              (prd ~+> pd) (Direction.Left)
//            )
//            case _ => Nil
//          }
//        ).flatten
//
//
//        val points = List(currentPoint, pointR, pointD, pointRD).flatten
//
//
//        List((points, edges))
//      }
//  ))
}
