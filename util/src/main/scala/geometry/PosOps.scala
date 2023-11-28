package geometry


trait PosOps[T <: PosOps[T]] {

  def +(other: T): T
  def -(other: T): T

  def manhattanDistance(other: T): BigInt

  
}
