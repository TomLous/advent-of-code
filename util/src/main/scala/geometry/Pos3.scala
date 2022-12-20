package geometry
import scala.annotation.targetName

case class Pos3(x: BigInt, y: BigInt, z: BigInt) extends PosOps[Pos3]:
  override def +(other: Pos3): Pos3                   = Pos3(x + other.x, y + other.y, z + other.z)
  override def -(other: Pos3): Pos3                   = Pos3(x - other.x, y - other.y, z - other.z)
  override def manhattanDistance(other: Pos3): BigInt = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
  
