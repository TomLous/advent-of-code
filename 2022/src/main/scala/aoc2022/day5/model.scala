package aoc2022.day5

object model {

  type Input = (Game, Game)

  case class Box(id: Char)
  case class StackRow(cols: Map[Int, Box])
  case class MoveInstruction(num: Int, from: Int, to: Int)

  case class Game(state: GameState, moves: List[MoveInstruction]):
    def followInstructions: GameState =
      moves.foldLeft(state)((newState, instruction) => newState.move(instruction))
  case class GameState(stacks: Map[Int, List[Box]]):
    def output: String = stacks.toList.sortBy(_._1).flatMap(_._2.headOption.map(_.id)).mkString

    def move(instruction: MoveInstruction): GameState =
      instruction match
        case MoveInstruction(num, from, to) =>
          stacks.get(from) match
            case Some(stack) =>
              stack.splitAt(num) match
                case (top, remaining) => GameState(stacks ++ Map(from -> remaining, to -> (top ++ stacks(to))))
            case None => throw new Exception("No box to move")

  object Game:
    def apply(rows: List[StackRow],  instructions: List[MoveInstruction], splitInstructions: Boolean = true): Game =
      val stacks = rows.flatMap(_.cols.toList).groupBy(_._1).map {
        case (col, boxes) => col+1 -> boxes.map(_._2)
      }

      val inst  = if (splitInstructions) instructions.flatMap { instruction =>
        (1 to instruction.num).map { i =>
          MoveInstruction(1, instruction.from, instruction.to)
        }
      } else instructions

      Game(GameState(stacks), inst)


}