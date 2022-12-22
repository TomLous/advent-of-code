package geometry

case class Pos2(x: BigInt, y: BigInt) extends PosOps[Pos2]:
  lazy val row:Int = y.toInt
  lazy val col:Int = x.toInt
  override def +(other: Pos2): Pos2                   = Pos2(x + other.x, y + other.y)
  override def -(other: Pos2): Pos2                   = Pos2(x - other.x, y - other.y)
  override def manhattanDistance(other: Pos2): BigInt = (x - other.x).abs + (y - other.y).abs
  