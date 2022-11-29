package aoc2021.day16

import zio.Chunk

import scala.util.Try

object model {

  def bits2Int(bits:List[Int]):Int = Integer.parseInt(bits.mkString, 2)
  def bits2Long(bits:List[Int]):Long = java.lang.Long.parseLong(bits.mkString,2)

  type Version = Int
  type TypeId = Int

  trait Packet:
    def version: Int
    def versionSum: Int
    def value: Long

  trait State:
    val level:Int = 0
    val position: Int
    val bits: List[Int]
    val process: Int => Either[State, Packet]

  trait PacketType:
    def typeId: Int

  case class PacketChunkStateResult(state: PacketChunkState, packets: Chunk[Packet])

  case class PacketChunkState(state: State=PacketState()):
    val processChunk: Chunk[Int] => PacketChunkStateResult = chunk => chunk.foldLeft(PacketChunkStateResult(this, Chunk.empty[Packet])) {
      case (PacketChunkStateResult(PacketChunkState(state), output), currenBit) =>
        state.process(currenBit) match {
          case Left(newState) => PacketChunkStateResult(PacketChunkState(newState), output)
          case Right(packet) => PacketChunkStateResult(PacketChunkState(PacketState()), output :+ packet)
          }
        }

  // Packet
  case class PacketState(override val position: Int=0, override val bits: List[Int]=Nil, override val level: Int=0) extends State:
      override def toString: String = {
        val prefix = "\t".repeat(level)
        s"$prefix $level PS: ${bits.mkString}"
      }

      private def parseVersionType(bit: Int):(Version, TypeId) = (bits :+ bit).grouped(3).toList match
        case versionBits :: typeBits :: Nil => (bits2Int(versionBits), bits2Int(typeBits))
        case _ => throw new Exception("Invalid version and type")

      override val process: Int => Either[State, Packet] = currenBit =>
        if (position == 5)
          parseVersionType(currenBit) match
            case (version, typeId) if typeId==Literal.typeId => Left(LiteralState(version=version, level=level))
            case (version, typeId) => Left(OperatorState(version=version, typeId=typeId, level=level))
            case _ => throw new Exception("Not implemented"); Left(PacketState())
        else
            Left(PacketState(position+1, bits :+ currenBit, level=level))


  // Literal
  case class Literal(override val value: Long, override val version: Int) extends Packet:
    override val versionSum: Int = version

  object Literal extends PacketType:
    override def typeId: Int = 4

    def apply(digits: List[Int], version: Int): Literal = Literal(bits2Long(digits), version)

  case class LiteralState(bits: List[Int]=Nil, override val position: Int=0, hasNext: Boolean = true, version: Version, override val level:Int=0) extends State:
    override def toString: String = {
      val prefix = "\t".repeat(level)
      s"$prefix $level Lit(v: $version)($position) - ${bits.mkString}"
    }

    override val process: Int => Either[LiteralState, Literal] = currenBit =>
      if(position % 5 == 0)
        Left(LiteralState(bits, position+1, hasNext = currenBit==1, version=version, level=level))
      else if(position % 5 == 4 && !hasNext)
        Right(Literal(bits :+ currenBit, version))
      else
        Left(LiteralState(bits :+ currenBit, position+1, hasNext=hasNext, version=version, level=level))

  trait Operator extends Packet:
    override val versionSum: Int = subPackets.map(_.versionSum).sum + version
    def subPackets: List[Packet]

  case class SumOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets.map(_.value).sum

  case class ProdOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets.map(_.value).product

  case class MinOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets.map(_.value).min

  case class MaxOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets.map(_.value).max

  case class GTOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets match
      case l :: r :: Nil => if(l.value > r.value) 1 else 0
      case _ => throw new Exception("Invalid number of subpackets")

  case class LTOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets match
      case l :: r :: Nil => if(l.value < r.value) 1 else 0
      case _ => throw new Exception("Invalid number of subpackets")

  case class EqOp(override val version: Int, override val subPackets: List[Packet]) extends Operator:
    override val value: Long = subPackets.map(_.value).distinct.size match
      case 1 => 1
      case _ => 0

  object Operator:
    def apply(version: Int, typeId: Int, subPackets: List[Packet]): Operator = typeId match
      case 0=> SumOp(version, subPackets)
      case 1 => ProdOp(version, subPackets)
      case 2 => MinOp(version, subPackets)
      case 3 => MaxOp(version, subPackets)
      case 5 => GTOp(version, subPackets)
      case 6 => LTOp(version, subPackets)
      case 7 => EqOp(version, subPackets)
      case _ => throw new Exception("Not implemented")

  case class OperatorState(bits: List[Int]=Nil, override val position: Int=0, lengthType:Int= -1, numSubPackets:Option[Int]=None, subPacketsEnd:Option[Int]= None, typeId: TypeId, version: Version, subPacketState:Option[State]=None, subPackets:List[Packet]=Nil, override val level:Int=0) extends State:
    override def toString: String = {
      val prefix = "\t".repeat(level)
      s"$prefix $level Op(v: $version, t: $typeId)($position)  lt: $lengthType | numsp: ${numSubPackets.getOrElse("-")} | spend: ${subPacketsEnd.getOrElse("-")} - ${bits.mkString}\n\t$prefix" + subPacketState.map(_.toString).getOrElse("-") + s"\n\t$prefix" + subPackets.map(_.toString).mkString("\n\t")
    }

    override val process: Int => Either[OperatorState, Operator] = currenBit =>
//      if(level == 0)
//        println(this.toString + "\n --- \n")

      if(position == 0)
        Left(OperatorState(bits, position+1, lengthType=currenBit, typeId=typeId, version=version, level=level))
      else if(lengthType == 1 && position == 11)
        Left(OperatorState(Nil, position+1, lengthType=lengthType, numSubPackets=Some(bits2Int(bits :+ currenBit)), typeId=typeId, version=version, subPacketState=Some(PacketState(level=level+1)), level=level))
      else if(lengthType == 0 && position == 15)
        Left(OperatorState(Nil, position+1, lengthType=lengthType, subPacketsEnd=Some(position  + bits2Int(bits :+ currenBit)), typeId=typeId, version=version, subPacketState=Some(PacketState(level=level+1)), level=level))
      else if(subPacketState.nonEmpty)
        subPacketState.get.process(currenBit) match {
          case Left(newSubState) =>
            Left(OperatorState(Nil, position+1,  lengthType=lengthType, numSubPackets=numSubPackets, subPacketsEnd=subPacketsEnd,typeId=typeId, version=version, subPacketState=Some(newSubState), subPackets=subPackets, level=level))
          case Right(newSubPacket) if subPacketsEnd.contains(position) || numSubPackets.contains(subPackets.length + 1)=>
            Right(Operator(version, typeId, subPackets :+ newSubPacket))
          case Right(newSubPacket) =>
            Left(OperatorState(Nil, position+1,  lengthType=lengthType, numSubPackets=numSubPackets, subPacketsEnd=subPacketsEnd, typeId=typeId,version=version, subPacketState=Some(PacketState(level = level+1)), subPackets=subPackets :+ newSubPacket, level=level))
        }
      else
        Left(OperatorState(bits :+ currenBit, position+1, lengthType=lengthType, numSubPackets=numSubPackets, subPacketsEnd=subPacketsEnd, typeId=typeId, version=version, level=level))



}
