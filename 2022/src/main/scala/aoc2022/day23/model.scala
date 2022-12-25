package aoc2022.day23

object model {

  case class Elf(x: BigInt, y: BigInt):
    private lazy val neighborList: IndexedSeq[Elf] = for {
      yOffset <- -1 to 1
      xOffset <- -1 to 1
      if !(xOffset == 0 && yOffset == 0)
    } yield Elf(x + xOffset, y + yOffset)

    lazy val Seq(northWest, north, northEast, west, east, southWest, south, southEast) = neighborList
    lazy val neighbors: Set[Elf]                                                       = neighborList.toSet

    lazy val northCheck: Set[Elf] = Set(northWest, north, northEast)
    lazy val southCheck: Set[Elf] = Set(southWest, south, southEast)
    lazy val westCheck: Set[Elf]  = Set(northWest, west, southWest)
    lazy val eastCheck: Set[Elf]  = Set(northEast, east, southEast)

    lazy val checks: List[(Elf, Set[Elf])] = List(
      (north, northCheck),
      (south, southCheck),
      (west, westCheck),
      (east, eastCheck)
    )

  case class Area(elves: Set[Elf], round: Int = 0):
    private lazy val (minX, maxX, minY, maxY) = (elves.map(_.x).min, elves.map(_.x).max, elves.map(_.y).min, elves.map(_.y).max)
    private lazy val (width, height)          = (maxX - minX + 1, maxY - minY + 1)

    lazy val score: BigInt = width * height - elves.size

    override def toString: String =
      val grid = for
        y <- minY to maxY
        x <- minX to maxX
      yield
        if elves.contains(Elf(x, y)) then "#"
        else "."
      s"== End of Round $round ==\n" +
      grid.mkString("").grouped(width.toInt).mkString("\n")
      + "\n"

    lazy val nextRound: Area = {

      val startIndex: Int = round % 4

      val moveAttempt: Map[Elf, Elf] = elves
        .map(elf => elf -> elf.neighbors.intersect(elves))
        .map {
          case (elf, neighbors) if neighbors.isEmpty => elf -> elf
          case (elf, neighbors) =>
            elf -> (0 until 4)
              .foldLeft(Option.empty[Elf]) {
                case (None, i) =>
                  val (newElf, check) = elf.checks((i + startIndex) % 4)
                  if (check.intersect(neighbors).isEmpty) Some(newElf)
                  else None
                case (e, _) => e
              }
              .getOrElse(elf)
        }
        .toMap

      val collisions: Map[Elf, Elf] = moveAttempt
        .groupBy(_._2)
        .filter(_._2.size > 1)
        .values
        .flatMap(_.keys.map(elf => elf -> elf))
        .toMap

      val newElves = (moveAttempt ++ collisions).values.toSet

      Area(newElves, round + 1)
    }

    def iterator: LazyList[Area] = LazyList.iterate(this)(_.nextRound)

  object Area:
    def fromList(input: List[List[Boolean]]): Area =
      val elves = for (
        y <- input.indices;
        x <- input(y).indices
        if input(y)(x)
      ) yield Elf(x, y)

      Area(elves.toSet, 0)

}
