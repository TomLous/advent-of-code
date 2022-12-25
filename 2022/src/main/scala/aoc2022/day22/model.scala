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
import zio.Tag

import scala.annotation.tailrec

object model {

  trait Point:
    def col: BigInt
    def row: BigInt

  case class Valid(row: BigInt, col: BigInt) extends Point:
    def score: BigInt = ((row) * 1000) + ((col) * 4)

  case class Wall(row: BigInt, col: BigInt)                                                   extends Point
  case class Empty(row: BigInt, col: BigInt)                                                  extends Point
  case class Border[P <: Point](row: BigInt, col: BigInt, point: P, orientation: Orientation) extends Point

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

    case Right extends Orientation(0)
    case Down  extends Orientation(1)
    case Left  extends Orientation(2)
    case Up    extends Orientation(3)

  type ValidEdge = LDiEdge[Valid] & EdgeCopy[LDiEdge] { type L1 = (Orientation, Orientation => Orientation) & Product }

  trait Command
  case class Move(steps: BigInt)    extends Command
  case class Turn(steps: Direction) extends Command

  case class GameBoard(commands: List[Command], rows: Int, cols: Int, nodes: List[Point], graph: Graph[Valid, LDiEdge]):

    lazy val wrapCube: GameBoard =
      val borders    = nodes.collect { case b: Border[_] => b }
      val allBorders = borders.toSet
      val nonBorders = nodes.collect {
        case b: Valid => b
        case b: Wall  => b
      }

      case class StitchState(b1: Border[_], b2: Border[_], b1Direction: Orientation, b2Direction: Orientation, remainingBorders: Set[Border[_]], edges: Set[ValidEdge] = Set.empty, done:Boolean=false)

      val faceLength = math.sqrt(nonBorders.length / 6).toInt

      val startStitching = borders.groupBy(b => (b.row, b.col)).values.filter(_.length > 1).map { case List(l, r) =>
        StitchState(l, r, Orientation.Up, Orientation.Left, allBorders)
      } // .map(_.sortBy(_.orientation.score)).toList

      println(startStitching.head)

      def findNext(current: Border[_], direction: Orientation, borders: Set[Border[_]]):Option[(Border[_], Orientation)] =
        direction match {
          case Orientation.Down  => borders.find(b => b.col == current.col && b.row == current.row + 1)
          case Orientation.Up    => borders.find(b => b.col == current.col && b.row == current.row - 1)
          case Orientation.Left  => borders.find(b => b.col == current.col - 1 && b.row == current.row)
          case Orientation.Right => borders.find(b => b.col == current.col + 1 && b.row == current.row)
        } match {
          case Some(b) =>
            Some((b, direction))
          case None =>
            borders.find(b => b.point == current.point && b != current) match
              case Some(newB) =>
                val td = if current.orientation.turn(Direction.Left) == newB.orientation then Direction.Left else Direction.Right
                val newO = direction.turn(td)
                Some((newB, newO))
              case None => None
        }





      def nextStitch(stitchState: StitchState) = stitchState match {
        case StitchState(b1 @ Border(b1Row, b1Col, pB1, b1Or), b2 @ Border(b2Row, b2Col, pB2, b2Or), b1Direction, b2Direction, remBorders, edges, done) =>
          val newEdges: Set[ValidEdge] = (pB1, pB2) match {
            case (valid1: Valid, valid2: Valid) =>
              Set(
                (valid1 ~+> valid2)(b2Or, GameBoard.checkBorderTurn(b1Or, b2Or)),
                (valid2 ~+> valid1)(b1Or, GameBoard.checkBorderTurn(b2Or, b1Or))
              )
            case _ => Set.empty
          }

          println("---")
          println(pB1)
          println(pB2)
          println(newEdges)

          (findNext(b1, b1Direction, remBorders), findNext(b2, b2Direction, remBorders)) match
            case (Some(nextB1, nextB1Direction), Some(nextB2, nextB2Direction)) =>
              val newBorders = remBorders - b1 - b2
              StitchState(nextB1, nextB2, nextB1Direction, nextB2Direction, newBorders, edges ++ newEdges, done =  done | newBorders.isEmpty)
            case _ =>
              StitchState(b1, b2, b1Direction, b2Direction, remBorders, edges, done = true)
      }

      val l         = LazyList.iterate(startStitching.head)(nextStitch)
      val lastState = l.dropWhile(!_.done).head

      lastState.edges.foreach(println)

//      val sticther = LazyList.iterate(startStitching.head)

//      def getPointRange(fromRow: BigInt, fromCol: BigInt, borderOrientation: Orientation): List[(BigInt, BigInt)] =
//        borderOrientation match
//          case Orientation.Up   => List.fill(faceLength)(fromRow) zip (fromCol - faceLength to fromCol)
//          case Orientation.Down => List.fill(faceLength)(fromRow) zip (fromCol - faceLength to fromCol)
//
//      val (usedBorders, firstStitchEdges) = startStitching.flatMap { case List(Border(b1Row, b1Col, valid1, b1Orientation), Border(b2Row, b2Col, valid2, b2Orientation)) =>
//        val fB1toB2 = GameBoard.checkBorderTurn(b1Orientation, b2Orientation)
//        val fB2toB1 = GameBoard.checkBorderTurn(b2Orientation, b1Orientation)
//
//        val zipBorders: List[(Border, Border)] = (b1Orientation, b2Orientation) match
//          case (Orientation.Right, Orientation.Down) => //  ┛
//            val b1List = ((b1Row - faceLength + 1) to b1Row).map(r => Border(r, b1Col, Valid(r, valid1.col), Orientation.Right)).toList
//            val b2List = ((b2Col - faceLength + 1) to b2Col).map(c => Border(b2Row, c, Valid(valid2.row, c), Orientation.Down)).toList
//            b1List.zip(b2List)
//          case (Orientation.Right, Orientation.Up) => // ┓
//            val b1List = (b1Row until (b1Row + faceLength)).map(r => Border(r, b1Col, Valid(r, valid1.col), Orientation.Right)).toList
//            val b2List = ((b2Col - faceLength + 1) to b2Col).map(c => Border(b2Row, c, Valid(valid2.row, c), Orientation.Up)).toList
//            b1List.zip(b2List)
//          case (Orientation.Left, Orientation.Up) => // ┏
//            val b1List = (b1Row until (b1Row + faceLength)).map(r => Border(r, b1Col, Valid(r, valid1.col), Orientation.Left)).toList
//            val b2List = (b2Col until (b2Col + faceLength)).map(c => Border(b2Row, c, Valid(valid2.row, c), Orientation.Up)).toList
//            b1List.zip(b2List)
//          case (Orientation.Down, Orientation.Left) => // ┗
//            val b1List = (b1Col until (b1Col + faceLength)).map(c => Border(b1Row, c, Valid(valid1.row, c), Orientation.Down)).toList
//            val b2List = ((b2Row - faceLength + 1) to b2Row).map(r => Border(r, b2Col, Valid(r, valid2.col), Orientation.Left)).toList
//            b1List.zip(b2List)
//
//        zipBorders.flatMap {
//          case (b1, b2) if borders.contains(b1) && borders.contains(b2) =>
//            List(
//              ((b1, b2, b1Orientation.reverse), (b1.valid ~+> b2.valid)(b2.orientation, fB1toB2)),
//              ((b2, b1, b2Orientation.reverse), (b2.valid ~+> b1.valid)(b1.orientation, fB2toB1))
//            )
//          case _ => Nil
//        }
//      }.unzip
//
//      val remainingBorders = allBorders -- usedBorders.map(_._1).toSet
//
//      val connectedThroughFirstStitch = remainingBorders.flatMap { rb => usedBorders.find(_._1.valid == rb.valid).map(pair => (rb, pair._2, pair._3)) }
//
//      connectedThroughFirstStitch.foreach(println)

      println(" --- ")

//      firstStitchEdges.foreach( println)
//
//      remainingBorders.toList.sortBy(_.col).sortBy(_.row).foreach(println)
//
//      println(s"$rows $cols / $faceLength") // 200 150 / 12 16
      // 200 150 / 12 16
      // 200 150 / 12 16
      // 15000

      /*
      case Right extends Orientation(0)
      case Down  extends Orientation(1)
      case Left  extends Orientation(2)
      case Up    extends Orientation(3)
       */
      /*
      real:
      List(Border(51,101,Valid(51,100),Left), Border(51,101,Valid(50,101),Up))
      List(Border(100,50,Valid(100,51),Right), Border(100,50,Valid(101,50),Down))
      List(Border(151,51,Valid(151,50),Left), Border(151,51,Valid(150,51),Up))

      test:
      List(Border(4,8,Valid(4,9),Right), Border(4,8,Valid(5,8),Down))
      List(Border(9,8,Valid(9,9),Right), Border(9,8,Valid(8,8),Up))
      List(Border(8,13,Valid(9,13),Down), Border(8,13,Valid(8,12),Left))

       */

      GameBoard(commands, rows, cols, nodes, graph)

    lazy val wrapAround: GameBoard = {
      val validBorders: List[Border[Valid]] = nodes.collect { case b @ Border(_, _, _: Valid, _) => b.asInstanceOf[Border[Valid]] }

      def findOppositePair(border: Border[Valid]): Option[(Border[Valid], Border[Valid])] = {
        val fCheck: Border[Valid] => Boolean =
          if (border.orientation == Orientation.Up || border.orientation == Orientation.Down)
            _.point.col == border.point.col
          else
            _.point.row == border.point.row

        validBorders
          .find(b => b.orientation == border.orientation.reverse && fCheck(b))
          .map(b => (border, b))
      }

      val wrapAroundEdges = validBorders
        .flatMap(findOppositePair)
        .flatMap { case (b1, b2) =>
          List(
            (b1.point ~+> b2.point)(b2.orientation, GameBoard.borderStraight),
            (b2.point ~+> b1.point)(b1.orientation, GameBoard.borderStraight)
          )
        }
        .distinct

      GameBoard(commands, rows, cols, nodes, graph ++ wrapAroundEdges)
    }

    private def node(valid: Valid): graph.NodeT = graph get valid

    private def move(node: graph.NodeT, steps: BigInt, orientation: Orientation): (graph.NodeT, Orientation) =
      @tailrec
      def step(currentNode: graph.NodeT, currentOrientation: Orientation, stepsRemaining: Int): (graph.NodeT, Orientation) =
        if stepsRemaining == 0 then (currentNode, currentOrientation)
        else
          currentNode.outgoing
            .find(_.label match {
              case (o, _) => o == currentOrientation
            })
            .map(e => (e.to, e.label)) match
            case Some((nextNode, lab: (Orientation, Orientation => Orientation))) =>
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
    val borderStraight: Orientation => Orientation  = identity
    val borderTurnLeft: Orientation => Orientation  = _.turn(Direction.Left)
    val borderTurnRight: Orientation => Orientation = _.turn(Direction.Right)

    def checkBorderTurn(from: Orientation, to: Orientation) = if from.turn(Direction.Left) == to then borderTurnLeft else borderTurnLeft

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
              (cp ~+> pr)(orientation, borderStraight),
              (pr ~+> cp)(orientation.reverse, borderStraight)
            )
          case _ => Nil
        }

      def makeBorder(from: Point, to: Point, orientation: Orientation) =
        (from, to) match {
          case (e: Empty, p @ (_: Wall | _: Valid)) => Some(Border(e.row, e.col, p, orientation))
          case (p @ (_: Wall | _: Valid), e: Empty) => Some(Border(e.row, e.col, p, orientation.reverse))
          case _                                    => None
        }

      var faceNum = 0
      val (nodeSeq, edgeSeq) = (" " :: field)
        .map(" " + _)                                   // prefix empty
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

              val edges: Set[ValidEdge] = Set(
                makeEdges(currentPoint, pointR, Orientation.Right),
                makeEdges(currentPoint, pointD, Orientation.Down),
                makeEdges(pointD, pointRD, Orientation.Right),
                makeEdges(pointR, pointRD, Orientation.Down)
              ).flatten

              val points = Set(currentPoint, pointR, pointD, pointRD).collect({
                case p: Valid => p
                case w: Wall  => w
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

      GameBoard(commands, field.length - 1, maxWidth - 2, nodes, Graph(edges: _*))
    }

}
