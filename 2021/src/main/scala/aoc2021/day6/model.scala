package aoc2021.day6

import scala.util.Try

object model {

  case class School(lanternFish: Map[Int, Long]):
    lazy val iterateOnce: School =
      School(lanternFish.map{
        case (age, count) if age == 0 => Map(6 -> count, 8 -> count)
        case (age, count) => Map(age - 1 -> count)
      }.reduce{
        case (map1, map2) => map1 ++ map2.map{
          case (age, count) => age -> (count + map1.getOrElse(age, 0L))
        }
      })

    lazy val size:Long = lanternFish.values.sum

    def iterate(iterations: Int):School = School.iterate(this, iterations)


  object School:
    def apply(lanternFishInit: List[Int]):School = {
      School(lanternFishInit.groupBy(identity).map{
        case (age, col) => age -> col.size.toLong
      }.toMap)
  }

    def iterate(initSchool: School, iterations: Int):School = {
      def iterateGroup(school:School, iterationsLeft: Int): School =
        if iterationsLeft == 0 then school
        else iterateGroup(school.iterateOnce, iterationsLeft - 1)

      iterateGroup(initSchool, iterations)
    }




}
