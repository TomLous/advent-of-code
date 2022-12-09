package aoc2022.day9

object model {

  case class Input(direction: Direction, distance: Int)

  enum Direction:
    case Up, Down, Left, Right

  object Direction:
    def apply(c: Char): Direction = c match
      case 'U' => Up
      case 'D' => Down
      case 'L' => Left
      case 'R' => Right

  case class Point(x: Int, y: Int):
    def move(input: Input) = input.direction match
      case Direction.Up    => Point(x, y + input.distance)
      case Direction.Down  => Point(x, y - input.distance)
      case Direction.Left  => Point(x - input.distance, y)
      case Direction.Right => Point(x + input.distance, y)

    def follow(otherPoint: Point): Point =
      val xDiff = x - otherPoint.x
      val yDiff = y - otherPoint.y
      val newX  = if (math.abs(xDiff) >= 2) otherPoint.x + xDiff.sign else otherPoint.x
      val newY  = if (math.abs(yDiff) >= 2) otherPoint.y + yDiff.sign else otherPoint.y
      Point(newX, newY)

    def touches(otherPoint: Point): Boolean =
      math.abs(x - otherPoint.x) <= 1 && math.abs(y - otherPoint.y) <= 1

  case class RopeBridge(input: List[Input]):
    lazy val exploded: List[Input] = input.flatMap(in => List.fill(in.distance)(1).map(d => Input(in.direction, d)))

    def run(numKnots: Int): (List[Point], Map[Int, List[Point]]) = {
      val knotsMap: Map[Int, List[Point]] = (0 until numKnots).map(i => i -> List(Point(0, 0))).toMap

      exploded
        .foldLeft((List(Point(0, 0)), knotsMap)) { case ((lastHead :: otherHeadPoints, knots), input) =>
          val sortedKnots = knots.toList.sortBy(_._1)
          val newHead     = lastHead.move(input)
          val (_, newKnotPoints) = sortedKnots.foldLeft((newHead, List.empty[(Int, Point)])) {
            case ((followingPoint, addedKnotPoints), (_, knotLastVisited :: _)) if knotLastVisited.touches(followingPoint) =>
              (knotLastVisited, addedKnotPoints)
            case ((followingPoint, addedKnotPoints), (knot, knotLastVisited :: _)) =>
              val newKnotPoint = knotLastVisited.follow(followingPoint)
              (newKnotPoint, (knot, newKnotPoint) :: addedKnotPoints)
          }
          val newKnots = knots.map { case (knot, currentKnotPoints) =>
            newKnotPoints.find(_._1 == knot).map(_._2) match
              case Some(newKnotPoint) => knot -> (newKnotPoint :: currentKnotPoints)
              case None               => knot -> currentKnotPoints
          }
          (newHead :: lastHead :: otherHeadPoints, newKnots)
        }
    }

}
