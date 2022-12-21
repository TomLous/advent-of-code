package aoc2022.day21

object model {

  type Op     = BigInt => BigInt
  type Result = Op | BigInt
  type Action = BigInt => BigInt => BigInt

  case class Monkey(name: String)

  sealed trait Input:
    def monkey: Monkey
    def eval(f: Monkey => Result): Result

  final case class MonkeyNumber(monkey: Monkey, num: BigInt) extends Input:
    def eval(f: Monkey => Result): Result = num

  final case class MonkeyOp(monkey: Monkey, monkeyA: Monkey, monkeyB: Monkey, op: Char) extends Input:
    def eval(f: Monkey => Result): Result = op match
      case '+' => f(monkeyA) + f(monkeyB)
      case '*' => f(monkeyA) * f(monkeyB)
      case '-' => f(monkeyA) - f(monkeyB)
      case '/' => f(monkeyA) / f(monkeyB)
      case '=' => f(monkeyA) <> f(monkeyB)

  case object X extends Op:
    def apply(x: BigInt): BigInt = x

  object Op:
    def findSolution(x: Result, y: Result): Result = (x, y) match
      case (x: BigInt, y: Op)               => y(x)
      case (x: Op, y: BigInt)               => x(y)
      case (x: BigInt, y: BigInt) if x == y => x
      case (x: Op, y: Op)                   => throw new Exception(s"No solution found $x = $y")
      case (x: BigInt, y: BigInt)           => throw new Exception(s"No solution found $x != $y")

    def action(x: Result, y: Result, action: Action, reverseL: Action, reverseR: Action): Result = (x, y) match
      case (x: Op, y: Op)         => x andThen y
      case (x: BigInt, y: Op)     => y compose reverseR(x)
      case (x: Op, y: BigInt)     => x compose reverseL(y)
      case (x: BigInt, y: BigInt) => action(x)(y)

  def add: Action  = x => y => x + y
  def mult: Action = x => y => x * y
  def subL: Action = x => y => x - y
  def subR: Action = x => y => y - x
  def divL: Action = x => y => x / y
  def divR: Action = x => y => y / x

  extension (opOrBigInt: Result)
    def +(other: Result): Result  = Op.action(opOrBigInt, other, add, subR, subR)
    def -(other: Result): Result  = Op.action(opOrBigInt, other, subL, add, subL)
    def *(other: Result): Result  = Op.action(opOrBigInt, other, mult, divR, divR)
    def /(other: Result): Result  = Op.action(opOrBigInt, other, divL, mult, divL)
    def <>(other: Result): Result = Op.findSolution(opOrBigInt, other)

  case class MonkeyMath(input: List[Input]):

    private def rec(currentMonkey: Monkey): Result =
      input.find(_.monkey == currentMonkey) match
        case Some(m) => m.eval(rec)
        case None    => X //  this will be the unknown variable (humn)

    lazy val monkeyValueRoot: BigInt =
      rec(Monkey("root")) match
        case b: BigInt => b
        case b: Op     => throw new Exception(s"No solution: $b")

    lazy val monkeyValueHuman: BigInt =
      val newInput = input.flatMap {
        case MonkeyOp(Monkey("root"), a, b, _) => Some(MonkeyOp(Monkey("root"), a, b, '='))
        case m if m.monkey.name == "humn"      => None
        case i                                 => Some(i)
      }

      MonkeyMath(newInput).monkeyValueRoot

}
