package aoc2021.day7

import scala.util.Try

object model {

  case class CrabPositions(positions: List[Int]):
    lazy val sortedPositions:List[Int] =  positions.sortWith(_ < _)
    lazy val median = sortedPositions(sortedPositions.size / 2)
    lazy val max = sortedPositions.last

    lazy val advancesDistances:Map[Int, Long] = {
      def loop(pos: Int, previousVal: Long=0):List[(Int, Long)] = {
        val currentVal = pos + previousVal
        val record = (pos , currentVal)
        if(pos == max) record :: Nil
        else
            record :: loop(pos + 1,currentVal)
      }
      loop(0)
    }.toMap




    def calculateFuelSimple(pos:Int):FuelResult = FuelResult(pos,positions.map(crab => Math.abs(crab.toLong - pos.toLong)).sum.toLong)





    def calculateFuelAdvanced(pos:Int):FuelResult = FuelResult(pos,positions.map(crab => advancesDistances( Math.abs(crab - pos))).sum)

    def calculateOptimalPosition(fuelCalcSimple:Boolean): FuelResult = {
      def optimalFuelLoop(pos:Int, bestSoFar: Option[FuelResult]=None, traverseDirection: Option[Int]=None):FuelResult = {
        if(pos < 0) bestSoFar.get
        else
          val current = if (fuelCalcSimple) then calculateFuelSimple(pos) else calculateFuelAdvanced(pos)
//          println(s"pos: $pos, current: $current, traverseDirection: $traverseDirection")
          (bestSoFar, traverseDirection) match {
            case (None, _) => optimalFuelLoop(pos - 1, Some(current), Some(-1)) vs optimalFuelLoop(pos + 1, Some(current), Some(1))
            case (Some(best), Some(dir)) if  current.fuel < best.fuel =>
              optimalFuelLoop(pos + dir, Some(current), traverseDirection)
            case (Some(best), _) => best
          }
      }

      optimalFuelLoop(median)
    }

  case class FuelResult(position:Int, fuel:Long):
    def vs(that: FuelResult):FuelResult = if(this.fuel < that.fuel) this else that





}
