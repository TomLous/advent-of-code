package aoc2022.day24
import scala.annotation.tailrec

object model {

  enum Direction:
    case Left, Right, Up, Down, None

  case class Point(x: Int, y: Int, z: Int = 0):
   def neighbors(nextZ: Int): Set[Point] = Set(
      Point(x - 1, y, nextZ),
      Point(x + 1, y, nextZ),
      Point(x, y - 1, nextZ),
      Point(x, y + 1, nextZ),
      Point(x, y, nextZ)
    )

  case class Blizzard(x: Int, y: Int, z: Int, direction: Direction):
    lazy val toPoint: Point = Point(x, y, z)

  case class Basin(blizzard: Set[Blizzard], width: Int, height: Int):
    lazy val maze: LazyList[Set[Blizzard]] = LazyList.iterate(blizzard)(nextLayer)
    lazy val blizzardMinX:Int                  = 1
    lazy val blizzardMaxX:Int                  = width - 2
    lazy val blizzardMinY:Int                  = 1
    lazy val blizzardMaxY:Int                  = height - 2
    lazy val cycleLength:Int                   = lcm(blizzardMaxX, blizzardMaxY)

    lazy val startPoint: Point = Point(1, 0)
    lazy val endPoint: Point = Point(width-2, height-1)

    private def lcm(a: Int, b: Int): Int = if (a == 0 || b == 0) 0 else Math.abs(a * b) / gcd(a, b)

    @tailrec
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    def nextLayer(currentBlizzard: Set[Blizzard]): Set[Blizzard] =
      currentBlizzard
        .map(blizzard =>
          blizzard.direction match
            case Direction.Left if blizzard.x == blizzardMinX  => Blizzard(blizzardMaxX, blizzard.y, blizzard.z + 1, Direction.Left)
            case Direction.Left                                => Blizzard(blizzard.x - 1, blizzard.y, blizzard.z + 1, Direction.Left)
            case Direction.Right if blizzard.x == blizzardMaxX => Blizzard(blizzardMinX, blizzard.y, blizzard.z + 1, Direction.Right)
            case Direction.Right                               => Blizzard(blizzard.x + 1, blizzard.y, blizzard.z + 1, Direction.Right)
            case Direction.Up if blizzard.y == blizzardMinY    => Blizzard(blizzard.x, blizzardMaxY, blizzard.z + 1, Direction.Up)
            case Direction.Up                                  => Blizzard(blizzard.x, blizzard.y - 1, blizzard.z + 1, Direction.Up)
            case Direction.Down if blizzard.y == blizzardMaxY  => Blizzard(blizzard.x, blizzardMinY, blizzard.z + 1, Direction.Down)
            case Direction.Down                                => Blizzard(blizzard.x, blizzard.y + 1, blizzard.z + 1, Direction.Down)
            case Direction.None                                => Blizzard(blizzard.x, blizzard.y, blizzard.z + 1, Direction.None)
        )

    def shortestPath(from: Point, to: Seq[Point]): Option[BigInt] = {
      val fullMazePoints    = maze.take(cycleLength.toInt).foldLeft(Set.empty[Point])((acc, blizzards) => acc ++ blizzards.map(_.toPoint))


      @tailrec
      def bfs(queue: List[Point], goal: Point, distance: Map[Point, BigInt] = Map.empty): Option[(Point, BigInt)] =
        if (queue.isEmpty) None
        else
          val (current, rest) = queue.head -> queue.tail
          if (current.x == goal.x && current.y == goal.y)
            distance.get(current).map(d => (current, d))
          else
            val nextZ = (current.z + 1) % cycleLength

            val candidates = current.neighbors(nextZ).filter(p => p.x >= 0 && p.y >= 0 && p.x < width && p.y < height)

            val nextPoints = candidates
              .filterNot(p => distance.contains(p)).diff(fullMazePoints)

            val newDistance = distance ++ nextPoints.map(p => p -> (distance(current) + 1))

            bfs(rest ++ nextPoints, goal, newDistance)

      val res = to.foldLeft(Option((from, BigInt(0)))){
        case (Some((start, total)), newGoal) =>
          bfs(List(start), newGoal, Map(start -> 0)).map(res => (res._1, total + res._2))
        case (None, _) => None
      }

      res.map(_._2)
    }

  object Basin:
    def fromLines(list: List[String]): Basin =
      val blizzardList = for {
        y <- list.indices
        x <- list(y).indices
        c = list(y)(x)
      } yield
        if c == '<' then Some(Blizzard(x, y, 0, Direction.Left))
        else if c == '>' then Some(Blizzard(x, y, 0, Direction.Right))
        else if c == '^' then Some(Blizzard(x, y, 0, Direction.Up))
        else if c == 'v' then Some(Blizzard(x, y, 0, Direction.Down))
        else if c == '#' then Some(Blizzard(x, y, 0, Direction.None))
        else None

      Basin(blizzardList.flatten.toSet, list.head.length, list.length)

}
