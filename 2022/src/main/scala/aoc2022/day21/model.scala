package aoc2022.day21

object model {


  case class Monkey(name: String)

  sealed trait Input:
    def monkey: Monkey
    def eval(f:  Monkey => Op | BigInt):Op | BigInt

  final case class MonkeyNumber(monkey: Monkey, num: BigInt) extends Input:
    def eval(f:  Monkey => Op | BigInt):Op | BigInt = num

  final case class MonkeyOp(monkey: Monkey, monkeyA: Monkey, monkeyB: Monkey, op: Char) extends Input:
    def eval(f: Monkey => Op | BigInt):Op | BigInt = op match
          case '+' => f(monkeyA) + f(monkeyB)
          case '*' => f(monkeyA) * f(monkeyB)
          case '-' => f(monkeyA) - f(monkeyB)
          case '/' => f(monkeyA) / f(monkeyB)
          case '=' => f(monkeyA) <> f(monkeyB)

  case class Op(f: BigInt=> BigInt)
  object X extends Op(identity)

  object Op:
    def findSolution(x: Op | BigInt, y: Op | BigInt): Op | BigInt = (x, y) match
        case (x:BigInt, y:Op) => y.f(x)
        case (x:Op, y:BigInt) => x.f(y)
        case (x:BigInt, y:BigInt) if x == y => x
        case (x:Op, y:Op) => throw new Exception(s"No solution found $x = $y")
        case (x:BigInt, y:BigInt) => throw new Exception(s"No solution found $x != $y")

    def action(x: Op | BigInt, y: Op | BigInt, action: BigInt => BigInt => BigInt, reverseL:  BigInt => BigInt => BigInt, reverseR:  BigInt => BigInt => BigInt): Op | BigInt = (x, y) match
        case (x:Op, y:Op) => Op(x.f andThen y.f)
        case (x:BigInt, y:Op) => Op(y.f compose reverseR(x))
        case (x:Op, y:BigInt) => Op(x.f compose reverseL(y))
        case (x:BigInt, y:BigInt) => action(x)(y)

    def add: BigInt => BigInt => BigInt = x => y => x + y
    def mult: BigInt => BigInt => BigInt = x => y => x * y
    def subL: BigInt => BigInt => BigInt = x => y => x - y
    def subR: BigInt => BigInt => BigInt = x => y => y - x
    def divL: BigInt => BigInt => BigInt = x => y => x / y
    def divR: BigInt => BigInt => BigInt = x => y => y / x

    extension (opOrBigInt: Op | BigInt)
      def +(other: Op | BigInt):Op | BigInt =  Op.action(opOrBigInt, other, add, subR, subR)
      def -(other: Op | BigInt):Op | BigInt =  Op.action(opOrBigInt, other, subL, add, subL)
      def *(other: Op | BigInt):Op | BigInt =  Op.action(opOrBigInt, other, mult, divR, divR)
      def /(other: Op | BigInt):Op | BigInt =  Op.action(opOrBigInt, other, divL, mult, divL)
      def <>(other: Op | BigInt):Op | BigInt =  Op.findSolution(opOrBigInt, other)




  case class MonkeyMath(input: List[Input]):

    def rec(currentMonkey: Monkey):Op | BigInt =
      input.find(_.monkey == currentMonkey) match
        case Some(m) => m.eval(rec)
        case None => X

    lazy val monkeyValueRoot: BigInt =
      rec(Monkey("root")) match
        case b: BigInt => b
        case b: Op => throw new Exception(s"No solution: $b")

    lazy val monkeyValueHuman: BigInt =
      val newInput = input.flatMap{
        case MonkeyOp(Monkey("root"), a, b, _) => Some(MonkeyOp(Monkey("root"), a, b, '='))
        case m if m.monkey.name == "humn" => None
        case i => Some(i)
      }

      MonkeyMath(newInput).monkeyValueRoot



}
