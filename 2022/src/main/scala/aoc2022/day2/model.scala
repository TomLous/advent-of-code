package aoc2022.day2
import aoc2022.day2.model.GameResult.Draw
import scalax.collection.Graph
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
object model{

  type Input = (Char, Char)

  case class Strategy(opponent: GameAction, response: GameAction):
    def play: Int = GameAction.outcome(opponent, response).value + response.value

  object Strategy:
    def apply(mapping:Map[Char, GameAction])(input: Input) : Strategy = input match
      case (opp, res)  =>
        Strategy(mapping(opp), mapping(res))

    def apply(mappingAction:Map[Char, GameAction], mappingResult:Map[Char, GameResult])(input: Input) : Strategy = input match
      case (opp, res)  =>
        val oppAction = mappingAction(opp)
        val desiredResult = mappingResult(res)
        Strategy(oppAction, GameAction.resultFor(oppAction, desiredResult))

  enum GameResult(val value: Int):
    case Win extends GameResult(6)
    case Lose extends GameResult(0)
    case Draw extends GameResult(3)

  enum GameAction(val value: Int):
    case Rock extends GameAction(1)
    case Paper extends GameAction(2)
    case Scissors extends GameAction(3)
  object GameAction:
    private val graph = Graph(
      GameAction.Rock ~> GameAction.Scissors,
      GameAction.Paper ~> GameAction.Rock,
      GameAction.Scissors ~> GameAction.Paper
    )
    private def node(action: GameAction):graph.NodeT = graph.get(action)
    def outcome(opponent: GameAction, self: GameAction): GameResult = {
      node(self).pathTo(node(opponent))(Visitor.empty) match
        case Some(path) =>
          if (path.length == 0) GameResult.Draw
          else if (path.length == 1) GameResult.Win
          else GameResult.Lose
        case None => throw new Exception("No path found")
    }
    def resultFor(opponentAction: GameAction, desiredResult: GameResult): GameAction =
      (desiredResult match
        case GameResult.Win => node(opponentAction).diPredecessors.head
        case GameResult.Draw => node(opponentAction)
        case GameResult.Lose => node(opponentAction).diSuccessors.head
        ).value




}
