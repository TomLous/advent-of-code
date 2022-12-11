package aoc2022.day11

object model {

  trait Input

  type Item = Long
  type MonkeyId = Int

  case class InpMonkeyId(id: Int) extends Input
  case class InpMonkeyItems(items: List[Int]) extends Input
  case class InpMonkeyOp(op: Char, value: String) extends Input:
    def toF: Item => Item = op match
      case '+' => if(value == "old") i => i + i else _ + value.toInt
      case '*' => if(value == "old") i => i * i  else _ * value.toInt

  case class InpMonkeyTest(divisibleBy: Int) extends Input
  case class InpMonkeyTestResult(test: Boolean, monkeyId: Int) extends Input

  case class Monkey(id: MonkeyId, items: List[Item], inspectF: Item => Item, testF: Item => MonkeyId, mod: Long, inspectionCounter: Long = 0):
    override def toString: String = s"Monkey $id: ${items.mkString(", ")}  (insp: $inspectionCounter)"

    def inspectAll(worryReductionF: Item => Item): (Monkey, List[(MonkeyId, Item)]) =
      (
        Monkey(id, Nil, inspectF, testF, mod, inspectionCounter + items.length),
        items.map(item =>
          val newItem = worryReductionF(inspectF(item))
          val newMonkey = testF(newItem)
          (newMonkey, newItem)
       )
      )

    def append(newItems: List[Item]): Monkey =
      Monkey(id, items ++ newItems, inspectF, testF, mod, inspectionCounter)


  object Monkey:
    def apply(inputItems: List[Input]): Monkey =
      val id = inputItems.collectFirst { case InpMonkeyId(id) => id }.get
      val items = inputItems.collectFirst { case InpMonkeyItems(items) => items.map(_.toLong) }.get
      val op = inputItems.collectFirst { case i:InpMonkeyOp => i.toF }.get
      val testDiv = inputItems.collectFirst { case InpMonkeyTest(div) => div }.get
      val testR = inputItems.collect { case InpMonkeyTestResult(tf, id) => tf -> id }.toMap
      val testF: Item => MonkeyId = i => if(i % testDiv == 0) then testR(true) else testR(false)

      Monkey(id, items, op, testF, testDiv)


  case class MonkeyGroup(monkeys: List[Monkey]):
    lazy val maxMod:Long = monkeys.map(_.mod).product
    override def toString: String = monkeys.mkString("\n")

    def round(worryReductionF: Item => Item): MonkeyGroup =
      val (almostNewMonkeys, stillToAppend) = monkeys.foldLeft((List.empty[Monkey], List.empty[(MonkeyId, Item)])){
        case ((currentMonkeys, currentItems), monkey) =>

          val (thisItems, otherItems) = currentItems.partition(_._1 == monkey.id)
          monkey.append(thisItems.map(_._2)).inspectAll(item => worryReductionF(item % maxMod)) match
            case (newMonkey, newItems) =>
              (currentMonkeys :+ newMonkey, otherItems ++ newItems)
      }

      val newMonkeys = almostNewMonkeys.map(m =>
        m.append(stillToAppend.filter(_._1 == m.id).map(_._2))
      )


      MonkeyGroup(newMonkeys)
  

  
}
