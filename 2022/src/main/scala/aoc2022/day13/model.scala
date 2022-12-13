package aoc2022.day13

object model {
  
  trait DistressSignal extends Comparable[DistressSignal]

  case class Num(int: Int) extends DistressSignal:
    override def toString: String = int.toString

    override def compareTo(o: DistressSignal): Int =
      o match
        case Num(thatInt) => int.compareTo(thatInt)
        case l:ListOfDistressSignals => ListOfDistressSignals(this).compareTo(l)


  case class ListOfDistressSignals(nums: List[DistressSignal]) extends DistressSignal:
    override def toString: String = s"[${nums.mkString(",")}]"
    
    def add(el: DistressSignal): ListOfDistressSignals = ListOfDistressSignals(nums :+ el)

    override def compareTo(o: DistressSignal): Int =
      o match
        case n:Num => this.compareTo(ListOfDistressSignals(n))
        case ListOfDistressSignals(thatNums) =>
          nums
            .zip(thatNums)
            .map(_.compareTo(_))
            .find(_ != 0) match
              case Some(i) => i
              case None => nums.size.compareTo(thatNums.size)
  


  object ListOfDistressSignals:
    def apply(l: ListOfDistressSignals): ListOfDistressSignals = ListOfDistressSignals(l.nums)
    def apply(n: Num): ListOfDistressSignals = ListOfDistressSignals(List(n))

}
