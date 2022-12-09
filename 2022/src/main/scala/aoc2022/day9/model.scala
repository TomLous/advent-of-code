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
    // consider moves as moves of 1 unit
    lazy val exploded: List[Input] = input.flatMap(in => List.fill(in.distance)(1).map(d => Input(in.direction, d)))

    def run(numKnots: Int): Map[Int, List[Point]] = {
      // init all knots
      val knotsMap: List[(Int, List[Point])] = (0 to numKnots).map(i => i -> List(Point(0, 0))).toList

      // for each 1 step move
      exploded
        .foldLeft(knotsMap) { case (headKnot :: tailKnots, input) =>
          // move the head
          val newHead = headKnot._2.head.move(input)

          // move the knots by folding over each knot's predecessor
          val (_, newKnotPoints) = tailKnots.foldLeft((newHead, List(0 -> newHead))) {
            // if the knot touches the knot it follow, don't move it
            case ((followingPoint, addedKnotPoints), (_, knotLastVisited :: _)) if knotLastVisited.touches(followingPoint) =>
              (knotLastVisited, addedKnotPoints)
            // other wise follow the knot based on the knot's predecessor
            case ((followingPoint, addedKnotPoints), (knot, knotLastVisited :: _)) =>
              val newKnotPoint = knotLastVisited.follow(followingPoint)
              (newKnotPoint, (knot, newKnotPoint) :: addedKnotPoints)
          }

          // add the new knot points to the map
          newKnotPoints.foldLeft(headKnot :: tailKnots) { case (accMap, (knot, point)) =>
            accMap.updated(knot, (knot, point :: accMap(knot)._2))
          }
        }
        .toMap
    }

}
