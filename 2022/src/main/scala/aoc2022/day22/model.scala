package aoc2022.day22

import breeze.linalg.*
import geometry.Pos2
import scalax.collection.Graph
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.*
import scalax.collection.edge.Implicits.*
import scalax.collection.mutable.Graph as MGraph

import scala.annotation.tailrec

object model {

  trait Point:
    def col: BigInt
    def row: BigInt

  case class Valid(row: BigInt, col: BigInt) extends Point:
    def score: BigInt = ((row) * 1000) + ((col) * 4)

  case class Wall(row: BigInt, col: BigInt)                                           extends Point
  case class Empty(row: BigInt, col: BigInt)                                          extends Point
  case class Border(row: BigInt, col: BigInt,  valid: Valid, orientation: Orientation) extends Point

  enum Direction:
    case Left, Right

  enum Orientation(val score: BigInt):
    def turn(direction: Direction): Orientation =
      if (direction == Direction.Left) this match
        case Up    => Left
        case Down  => Right
        case Left  => Down
        case Right => Up
      else
        this match
          case Up    => Right
          case Down  => Left
          case Left  => Up
          case Right => Down

    def reverse: Orientation = this match
      case Up    => Down
      case Down  => Up
      case Left  => Right
      case Right => Left

    case Up    extends Orientation(3)
    case Down  extends Orientation(1)
    case Left  extends Orientation(2)
    case Right extends Orientation(0)

  type ValidEdge = LDiEdge[Valid] & EdgeCopy[LDiEdge] { type L1 = (Orientation, Orientation => Orientation)  & Product }

  trait Command
  case class Move(steps: BigInt)    extends Command
  case class Turn(steps: Direction) extends Command

  case class GameBoard(commands: List[Command], nodes: List[Point], graph: Graph[Valid, LDiEdge]):

    lazy val wrapCube: GameBoard =
      val borders = nodes.collect { case b: Border => b }

      borders.groupBy(b => (b.row, b.col)).values.filter(_.length > 1).foreach(println)

      GameBoard(commands,  nodes, graph)



    lazy val wrapAround: GameBoard =
      val borders = nodes.collect { case b: Border => b }

      val borderEdges: List[ValidEdge] = borders
        .flatMap {
          case border if border.orientation == Orientation.Up || border.orientation == Orientation.Down =>
            borders.find(b => b.orientation == border.orientation.reverse && b.valid.col == border.valid.col).map(b => (border, b))
          case border if border.orientation == Orientation.Left || border.orientation == Orientation.Right =>
            borders.find(b => b.orientation == border.orientation.reverse && b.valid.row == border.valid.row).map(b => (border, b))
        }
        .flatMap { case (b1, b2) =>
          List(
            (b1.valid ~+> b2.valid)(b2.orientation, GameBoard.borderIdentity),
            (b2.valid ~+> b1.valid)(b1.orientation, GameBoard.borderIdentity)
          )
        }
        .distinct

      GameBoard(commands,  nodes, graph ++ borderEdges)

    private def node(valid: Valid): graph.NodeT = graph get valid

    private def move(node: graph.NodeT, steps: BigInt, orientation: Orientation): (graph.NodeT, Orientation) =
      @tailrec
      def step(currentNode: graph.NodeT, currentOrientation: Orientation, stepsRemaining: Int): (graph.NodeT, Orientation) =
        if stepsRemaining == 0 then (currentNode, currentOrientation)
        else
          currentNode.outgoing.find(_.label match {
            case (o, _) => o == currentOrientation
          }).map(e => (e.to, e.label)) match
            case Some((nextNode, lab:(Orientation, Orientation => Orientation))) =>
              val orientationF = lab._2
              step(nextNode, orientationF(currentOrientation), stepsRemaining - 1)
            case None => (currentNode, currentOrientation)

      step(node, orientation, steps.toInt)

    lazy val runCommands: (Valid, Orientation) = {
      val start = nodes.collectFirst({ case v: Valid => v }).get // nodes are sorted so this is the first valid node
      @tailrec
      def followCommands(commandList: List[Command], currentNode: graph.NodeT, currentOrientation: Orientation): (Valid, Orientation) = commandList match {
        case Nil => (currentNode.value, currentOrientation)
        case Move(steps) :: remainingCommands =>
          val (nextNode, nextOrientation) = move(currentNode, steps, currentOrientation)
          followCommands(remainingCommands, nextNode, nextOrientation)
        case Turn(direction) :: remainingCommands =>
          followCommands(remainingCommands, currentNode, currentOrientation.turn(direction))
      }

      followCommands(commands, node(start), Orientation.Right)
    }

  object GameBoard:
    val borderIdentity: Orientation => Orientation = a =>  a

    private def parseCommands(str: String): List[Command] =
      """(\d+|[LR])""".r
        .findAllIn(str)
        .matchData
        .map(_.group(1) match
          case "L" => Turn(Direction.Left)
          case "R" => Turn(Direction.Right)
          case s   => Move(s.toInt)
        )
        .toList

    def apply(input: List[String]): GameBoard = {
      val commands = parseCommands(input.last)

      val field    = input.init
      val maxWidth = field.map(_.length).max + 2 // add border around for easier processing + border detection

      def getPoint(char: Char, row: BigInt, col: BigInt): Point =
        if (char == '.') Valid(row, col)
        else if (char == '#') Wall(row, col)
        else Empty(row, col) // used only for border detection

      def makeEdges(from: Point, to: Point, orientation: Orientation) =
        (from, to) match {
          case (cp: Valid, pr: Valid) =>
            List(
              (cp ~+> pr)(orientation, borderIdentity),
              (pr ~+> cp)(orientation.reverse, borderIdentity)
            )
          case _ => Nil
        }

      def makeBorder(from: Point, to: Point, orientation: Orientation) =
        (from, to) match {
          case (e: Empty, p: Valid) => Some(Border(e.row, e.col, p, orientation))
          case (p: Valid, e: Empty) => Some(Border(e.row, e.col, p, orientation.reverse))
          case _                    => None
        }

      var faceNum = 0
      val (nodeSeq, edgeSeq) = (" " :: field)
        .map(" " + _) //prefix empty
        .map(_.padTo(maxWidth, ' ').toCharArray.toList) // pad to max width with spaces
        .zipWithIndex
        .sliding(2)
        .map { case List((currentRow, currentRowIdx), (nextRow, nextRowIdx)) =>
          val (rowPoints, rowEdges) = currentRow.zipWithIndex
            .sliding(2)
            .toList
            .map { case List((currentChar, currentColIdx), (nextChar, nextColIdx)) =>
              val currentPoint = getPoint(currentChar, currentRowIdx, currentColIdx)
              val pointR       = getPoint(nextChar, currentRowIdx, nextColIdx)
              val pointD       = getPoint(nextRow(currentColIdx), nextRowIdx, currentColIdx)
              val pointRD      = getPoint(nextRow(nextColIdx), nextRowIdx, nextColIdx)

              val borders = List(
                makeBorder(currentPoint, pointR, Orientation.Right),
                makeBorder(currentPoint, pointD, Orientation.Down),
                makeBorder(pointD, pointRD, Orientation.Right),
                makeBorder(pointR, pointRD, Orientation.Down)
              )

              val edges: Set[
                LDiEdge[Valid] & EdgeCopy[LDiEdge] {
                  type L1 = (Orientation, Orientation => Orientation) & Product
                }
              ] = Set(
                makeEdges(currentPoint, pointR, Orientation.Right),
                makeEdges(currentPoint, pointD, Orientation.Down),
                makeEdges(pointD, pointRD, Orientation.Right),
                makeEdges(pointR, pointRD, Orientation.Down)
              ).flatten

              val points = Set(currentPoint, pointR, pointD, pointRD).collect({
                case p: Valid  => p
              }) ++ borders.flatten

              (points, edges)
            }
            .unzip
          (rowPoints.flatten.toSet, rowEdges.flatten.toSet)
        }
        .toList
        .unzip

      val nodes = nodeSeq.flatten.distinct.sortBy(_.col).sortBy(_.row)
      val edges = edgeSeq.flatten.distinct

      GameBoard(commands, nodes, Graph(edges: _*))
    }

}
