package aoc2020.day4
import scala.collection.immutable.BitSet

object model {

  sealed trait Input:
    def typeFlag: BitSet
    def valid: Boolean

  final case class BirthYear(value: Int) extends Input:
    lazy val typeFlag: BitSet = BitSet(1)
    lazy val valid: Boolean = value >= 1920 && value <= 2002

  final case class IssueYear(value: Int) extends Input:
    lazy val typeFlag: BitSet = BitSet(2)
    lazy val valid: Boolean = value >= 2010 && value <= 2020

  final case class ExpirationYear(value: Int) extends Input:
    lazy val typeFlag: BitSet = BitSet(3)
    lazy val valid: Boolean = value >= 2020 && value <= 2030

  final case class Height(value: Int, unit:String) extends Input:
    lazy val typeFlag: BitSet = BitSet(4)
    lazy val valid: Boolean = unit match
      case "cm" => value >= 150 && value <= 193
      case "in" => value >= 59 && value <= 76
      case _ => false

  final case class HairColor(value: String) extends Input:
    lazy val typeFlag: BitSet = BitSet(5)
    lazy val valid: Boolean = value.matches("#[0-9a-f]{6}")

  final case class EyeColor(value: String) extends Input:
    lazy val typeFlag: BitSet = BitSet(6)
    lazy val valid: Boolean = value.matches("amb|blu|brn|gry|grn|hzl|oth")

  final case class PassportId(value: String) extends Input:
    lazy val typeFlag: BitSet = BitSet(7)
    lazy val valid: Boolean = value.matches("[0-9]{9}")

  final case class CountryId(value: Int) extends Input:
    lazy val typeFlag: BitSet = BitSet(8)
    lazy val valid: Boolean = true

  case class Passport(list: List[Input]) {

    def check(bs: BitSet): Boolean =
      val valid = BitSet(1, 2, 3, 4, 5, 6, 7)
      (bs & valid) == valid

    def isValidPart1: Boolean =
      check(list.map(_.typeFlag).fold(BitSet.empty)(_ | _))

    def isValidPart2: Boolean =
      check(list.filter(_.valid).map(_.typeFlag).fold(BitSet.empty)(_ | _))
  }
}
