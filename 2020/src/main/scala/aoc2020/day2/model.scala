package aoc2020.day2

object model {

  type Input = (PasswordPolicy,String)

  case class PasswordPolicy(min: Int, max: Int, char: Char):
    lazy val range = min to max

    def isValidSimple(password: String): Boolean =
      range.contains(password.count(_ == char))

    def isValidComplex(password: String): Boolean =
      (password(min-1) == char) ^ (password.charAt(max-1) == char)
}
