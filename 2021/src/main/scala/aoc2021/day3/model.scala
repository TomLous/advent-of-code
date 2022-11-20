package aoc2021.day3

import scala.util.Try

object model {

  case class PowerConsumption(gamma: Long, epsilon: Long):
    lazy val mult:Long = gamma * epsilon


  case class LifeSupport(oxygen: Long, scrubber: Long):
    lazy val mult:Long = oxygen * scrubber


  case class BinaryTracker(sum: List[Int], size: Long):
    lazy val treshhold = size / 2
    lazy val mostCommon: List[Int] = sum.map(_ >= treshhold).map(if _ then 1 else 0)
    lazy val leastCommon: List[Int] = mostCommon.map(1 - _)
    lazy val gamma: Int = Integer.parseInt(mostCommon.mkString, 2)
    lazy val epsilon:Int = Integer.parseInt(leastCommon.mkString, 2)
    lazy val powerConsumption: PowerConsumption = PowerConsumption(gamma, epsilon)


  trait Tree:
    def propagate(binaryNum: List[Int]):Tree
    def size:Int
    def mostCommonFiltered: List[Int]
    def leastCommonFiltered: List[Int]

  case class Leaf(override val size:Int = 0) extends Tree:
    override def propagate(binaryNum: List[Int]):Tree= if(binaryNum.isEmpty) Leaf(size + 1) else Branch().propagate(binaryNum)
    override lazy val mostCommonFiltered: List[Int] = List.empty
    override lazy val leastCommonFiltered: List[Int] = List.empty

  case class Branch(`0`: Tree=Leaf(), `1`: Tree=Leaf(), override val size:Int=0) extends Tree:
    override def propagate(binaryNum: List[Int]):Branch=
      binaryNum.headOption match
        case Some(0) => Branch(`0`.propagate(binaryNum.tail), `1`, size+1)
        case Some(1) => Branch(`0`, `1`.propagate(binaryNum.tail), size+1)
        case _ => this //should never happen

    override lazy val mostCommonFiltered: List[Int] = if(`1`.size >= `0`.size) 1 :: `1`.mostCommonFiltered else 0 :: `0`.mostCommonFiltered
    override lazy val leastCommonFiltered: List[Int] = if((`1`.size >= `0`.size && `0`.size > 0) || `1`.size == 0) 0 :: `0`.leastCommonFiltered else 1 :: `1`.leastCommonFiltered
    val o2: Int = Integer.parseInt(mostCommonFiltered.mkString, 2)
    val co2: Int = Integer.parseInt(leastCommonFiltered.mkString, 2)
    lazy val lifeSupport: LifeSupport = LifeSupport(o2, co2)

}
