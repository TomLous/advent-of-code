package aoc2022.day17

object Day17:
  val shapes = Seq(
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)),
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)),
    Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),
    Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1)))

  case class Point(x: Int, y: Int)

  extension (shape: Set[Point])
    def move(dx: Int, dy: Int): Set[Point] = shape.map(p => Point(p.x + dx, p.y + dy))
    def canMove(grid: Set[Point]): Boolean = shape.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))

  case class State(jets: String, grid: Set[Point], shapeIndex: Int, jetIndex: Int, height: Int):
    def step: State =
      val initialShape = shapes(shapeIndex % shapes.size).move(3, height + 4)
      val (nextShape, nextJetIndex) = fall(initialShape, jetIndex)
      val nextHeight = height.max(nextShape.map(_.y).max)
      State(jets, grid ++ nextShape, shapeIndex + 1, nextJetIndex, nextHeight)

    def fall(shape: Set[Point], jetIndex: Int): (Set[Point], Int) =
      val jet = jets(jetIndex % jets.length)
      val first = if jet == '>' then shape.move(1, 0) else shape.move(-1, 0)
      val second = if first.canMove(grid) then first else shape
      val third = second.move(0, -1)
      if third.canMove(grid) then fall(third, jetIndex + 1) else (second, jetIndex + 1)
  end State

  def simulate(jets: String): Iterator[Int] =
    val initial = State(jets, Set.tabulate(8)(Point(_, 0)), 0, 0, 0)
    Iterator.iterate(initial)(_.step).map(_.height)

  def part1(input: String): Int = simulate(input).drop(2022).next()

  def part2(input: String): Long =
    val guess = 1000
    val height = simulate(input).slice(1, 5 * guess).toSeq
    val delta = height.sliding(2).map(s => s.last - s.head).toSeq
    val end = delta.size - guess
    val start = delta.lastIndexOfSlice(delta.takeRight(guess), end - 1)
    val cycleHeight = height(end) - height(start)
    val cycleWidth = end - start
    val offset = 1000000000000L - 1 - start
    val quotient = offset / cycleWidth
    val remainder = offset % cycleWidth
    (quotient * cycleHeight) + height(start + remainder.toInt)

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("aoc2022/day17/puzzle-input.txt").mkString.trim
    println(part1(data))
    println(part2(data))