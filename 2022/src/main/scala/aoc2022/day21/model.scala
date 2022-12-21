package aoc2022.day21

object model {

  case class Monkey(name: String)

  sealed trait Input:
    def monkey: Monkey

  final case class MonkeyNumber(monkey: Monkey, num: BigInt) extends Input
  final case class MonkeyOp(monkey: Monkey, monkeyA: Monkey, monkeyB: Monkey, op: Char) extends Input

  case class Op(value: String, f: BigInt=> BigInt)


  object Op:
    def add(x: Op | BigInt, y: Op | BigInt): Op | BigInt = (x, y) match
        case (x:Op, y:Op) => Op(s"(${x.value} + ${y.value})", x.f andThen y.f)
        case (x:BigInt, y:Op) => Op(s"($x + ${y.value})", y.f compose (newY =>
          println(s"step $newY -  $x = ${newY - x}")
          newY - x)
        )
        case (x:Op, y:BigInt) => Op(s"(${x.value} + $y)" , x.f compose (newX =>
          println(s"step $newX -  $y = ${newX - y}")
          newX - y
        ))
        case (x:BigInt, y:BigInt) => x + y

    def sub(x: Op | BigInt, y: Op | BigInt): Op | BigInt = (x, y) match
        case (x:Op, y:Op) => Op(s"(${x.value} - ${y.value})", x.f andThen y.f)
        case (x:BigInt, y:Op) => Op(s"($x - ${y.value})", y.f compose (newY =>
          println(s"step $x -  $newY = ${x - newY}")
             x- newY)
        )
        case (x:Op, y:BigInt) => Op(s"(${x.value} - $y)" , x.f compose (newX =>
          println(s"step $newX +  $y = ${newX + y}")
            newX + y
          ))
        case (x:BigInt, y:BigInt) => x - y

    def mult(x: Op | BigInt, y: Op | BigInt): Op | BigInt = (x, y) match
        case (x:Op, y:Op) => Op(s"(${x.value} * ${y.value})", x.f andThen y.f)
        case (x:BigInt, y:Op) => Op(s"($x * ${y.value})", y.f compose (newY =>
          println(s"step $newY /  $x = ${newY / x}")
            newY / x)
        )
        case (x:Op, y:BigInt) => Op(s"(${x.value} * $y)" , x.f compose (newX =>
          println(s"step $newX /  $y = ${newX / y}")
            newX / y
          ))
        case (x:BigInt, y:BigInt) => x * y

    def div(x: Op | BigInt, y: Op | BigInt): Op | BigInt = (x, y) match
        case (x:Op, y:Op) => Op(s"(${x.value} / ${y.value})", x.f andThen y.f)
        case (x:BigInt, y:Op) => Op(s"($x / ${y.value})", y.f compose (newY =>
          println(s"step $newY *  $x =  ${newY * x}")
            newY * x)
        )
        case (x:Op, y:BigInt) => Op(s"(${x.value} / $y)" , x.f compose (newX =>
          println(s"step $newX *  $y = ${newX * y}")
            newX * y
          ))
        case (x:BigInt, y:BigInt) => x / y

    def findSolution(x: Op | BigInt, y: Op | BigInt): Op | BigInt = (x, y) match
        case (x:Op, y:Op) => throw new Exception(s"No solution found $x $y")
        case (x:BigInt, y:Op) =>
          println(s"Found solution ${x} == ${y.value}")
          y.f(x)
        case (x:Op, y:BigInt) =>
          println(s"Found solution ${x.value} == ${y}")
          x.f(y)
        case (x:BigInt, y:BigInt) => throw new Exception(s"No solution found $x $y")





  case class MonkeyMath(input: List[Input]):

    lazy val monkeyValueRoot: BigInt =
      def rec(currentMonkey: Monkey):BigInt =
        input.find(_.monkey == currentMonkey) match
          case Some(MonkeyNumber(_, num)) => num
          case Some(MonkeyOp(_, a, b, op)) =>
            op match
              case '+' => rec(a) + rec(b)
              case '*' => rec(a) * rec(b)
              case '-' => rec(a) - rec(b)
              case '/' => rec(a) / rec(b)
          case None => throw new Exception(s"Monkey $currentMonkey not found")
      rec(Monkey("root"))

    lazy val monkeyValueHuman: BigInt =
      val human = Monkey("humn")
      val root = Monkey("root")

      val rootOp = input.find(_.monkey == root) match
        case Some(MonkeyOp(_, a, b, op)) => MonkeyOp(root, a, b, '=')
        case _ => throw new Exception(s"Monkey $root not found")

      val newInput = input
        .filter(_.monkey != human)
        .filter(_.monkey != root)
        .appended(rootOp)




      def rec(currentMonkey: Monkey):Op | BigInt =
        newInput.find(_.monkey == currentMonkey) match
          case Some(MonkeyNumber(_, num)) => num
          case Some(MonkeyOp(_, a, b, op)) =>
            op match
              case '+' => Op.add(rec(a),rec(b))
              case '*' => Op.mult(rec(a),rec(b))
              case '-' => Op.sub(rec(a),rec(b))
              case '/' => Op.div(rec(a),rec(b))
              case '=' => Op.findSolution(rec(a),rec(b))
          case None =>
            Op("x",  identity)

      rec(Monkey("root")) match
        case b: BigInt => b
        case b: Op => throw new Exception(s"No solution 1x op $b")

}
