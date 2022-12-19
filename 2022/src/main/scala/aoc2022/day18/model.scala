package aoc2022.day18


object model {

  case class Cube(x: Int, y: Int, z: Int):
    lazy val max: Int = x max y max z
    lazy val min: Int = x min y min z
    def inScope(minB:Int, maxB: Int):Boolean = max <= maxB && min >= minB
    lazy val neighbors: Set[Cube] =
      Set(1, -1).flatMap(diff => Set(Cube(x + diff, y, z), Cube(x, y + diff, z), Cube(x, y, z + diff)))

  case class Lava(list: Set[Cube]):
    lazy val maxDistance: Int = list.map(_.max).max + 1
    lazy val minDistance: Int = list.map(_.min).min - 1

    def exposed(ignorePockets: Boolean): Long =
      if (ignorePockets)
        def bfs(start: Cube): Set[Cube] =
          def rec(queue: Set[Cube], visited: Set[Cube] = Set.empty): Set[Cube] =
            if (queue.isEmpty) visited
            else
              val (current, rest) = queue.head -> queue.tail
              val newNeighbors = current.neighbors
                .diff(list)
                .diff(visited)
                .filter(_.inScope(minDistance, maxDistance))
              rec(rest ++ newNeighbors, visited + current)
          rec(Set(start))

        val exteriorCubes = bfs(Cube(minDistance, minDistance, minDistance))

        list.toList.map(_.neighbors.diff(list).intersect(exteriorCubes).size).sum
      else
        list.toList.map(_.neighbors.diff(list).size).sum

}
