package aoc2022.day19

object model {

  trait Robot:
    def make(mineralState: MineralState, robotState: RobotState): Option[(MineralState, RobotState)]

  case class OreRobot(ore: Int) extends Robot:
    def make(mineralState: MineralState, robotState: RobotState): Option[(MineralState, RobotState)] =
      if (mineralState.oreDelved >= ore)
        Some(
          mineralState.copy(oreDelved = mineralState.oreDelved - ore),
          robotState.copy(oreRobots = robotState.oreRobots + 1)
        )
      else None

  case class ClayRobot(ore: Int) extends Robot:
    def make(mineralState: MineralState, robotState: RobotState): Option[(MineralState, RobotState)] =
      if (mineralState.oreDelved >= ore)
        Some(
          mineralState.copy(oreDelved = mineralState.oreDelved - ore),
          robotState.copy(clayRobots = robotState.clayRobots + 1)
        )
      else None

  case class ObsidianRobot(ore: Int, clay: Int) extends Robot:
    def make(mineralState: MineralState, robotState: RobotState): Option[(MineralState, RobotState)] =
      if (mineralState.oreDelved >= ore && mineralState.clayDelved >= clay)
        Some(
          mineralState.copy(oreDelved = mineralState.oreDelved - ore, clayDelved = mineralState.clayDelved - clay),
          robotState.copy(obsidianRobots = robotState.obsidianRobots + 1)
        )
      else None

  case class GeodeRobot(ore: Int, obsidian: Int) extends Robot:
    def make(mineralState: MineralState, robotState: RobotState): Option[(MineralState, RobotState)] =
      if (mineralState.oreDelved >= ore && mineralState.obsidianDelved >= obsidian)
        Some(
          mineralState.copy(oreDelved = mineralState.oreDelved - ore, obsidianDelved = mineralState.obsidianDelved - obsidian),
          robotState.copy(geodeRobots = robotState.geodeRobots + 1)
        )
      else None

  case class MineralState(
    oreDelved: Long = 0L,
    clayDelved: Long = 0L,
    obsidianDelved: Long = 0L,
    geodeDelved: Long = 0L
  ):
    override def toString: String = s"$oreDelved ore\t\t\t$clayDelved clay\t\t\t$obsidianDelved obsidian\t$geodeDelved geode"

    def allMax(other: MineralState): Set[MineralState] =
      if (oreDelved >= other.oreDelved && clayDelved >= other.clayDelved && obsidianDelved >= other.obsidianDelved && geodeDelved >= other.geodeDelved)
        Set(this)
      else if (oreDelved < other.oreDelved && clayDelved < other.clayDelved && obsidianDelved < other.obsidianDelved && geodeDelved < other.geodeDelved)
        Set(other)
      else
        Set(this, other)

  case class RobotState(
    oreRobots: Long = 0L,
    clayRobots: Long = 0L,
    obsidianRobots: Long = 0L,
    geodeRobots: Long = 0L
  ):
    override def toString: String = s"$oreRobots ore robots\t$clayRobots clay robots\t$obsidianRobots robots\t$geodeRobots geode robots"

  case class BlueprintState(blueprint: Blueprint, mineralState: MineralState, robotState: RobotState):
    override def toString: String =
      s"""--------
        |Blueprint: ${blueprint.num} [${arbritraryScore}]
        |ore robots: ${robotState.oreRobots} => ${mineralState.oreDelved}
        |clay robots: ${robotState.clayRobots} => ${mineralState.clayDelved}
        |obsidian robots: ${robotState.obsidianRobots} => ${mineralState.obsidianDelved}
        |geode robots: ${robotState.geodeRobots} => ${mineralState.geodeDelved}
        |""".stripMargin
    def score: Long = mineralState.geodeDelved * blueprint.num
    lazy val arbritraryScore: Long = (mineralState.geodeDelved * 100000 + robotState.geodeRobots * 50000) +
      (mineralState.obsidianDelved * 1000 + robotState.obsidianRobots * 500) +
      (mineralState.clayDelved * 10 + robotState.clayRobots * 5) +
      (mineralState.oreDelved * 2 + robotState.oreRobots)

    lazy val makeOreRobot: Option[(MineralState, RobotState)] =
      if (robotState.oreRobots < blueprint.maxOreExpensePerMinute)
        blueprint.ore.make(mineralState, robotState)
      else None

    lazy val makeClayRobot: Option[(MineralState, RobotState)] =
      if (robotState.clayRobots < blueprint.maxClayExpensePerMinute)
        blueprint.clay.make(mineralState, robotState)
      else None

    lazy val makeObsidianRobot: Option[(MineralState, RobotState)] =
      if (robotState.obsidianRobots < blueprint.maxObsidianExpensePerMinute)
        blueprint.obsidian.make(mineralState, robotState)
      else None

    lazy val makeGeodeRobot: Option[(MineralState, RobotState)] = blueprint.geode.make(mineralState, robotState)

    lazy val skipBuildRobot: Option[(MineralState, RobotState)] =
      if (
        (robotState.clayRobots > 0 && mineralState.clayDelved < blueprint.maxClayExpensePerMinute) ||
        (robotState.oreRobots > 0 && mineralState.oreDelved < blueprint.maxOreExpensePerMinute) ||
        (robotState.obsidianRobots > 0 && mineralState.obsidianDelved < blueprint.maxObsidianExpensePerMinute)
      )
        Some(mineralState, robotState)
      else None

    lazy val minuteStep: Set[BlueprintState] =
      makeGeodeRobot
        .map(Set(_))
        .getOrElse(
          List(makeObsidianRobot, makeClayRobot, makeOreRobot, skipBuildRobot).flatten.toSet
        )
        .map { case (newMineralState, newRobotState) =>
          BlueprintState(
            blueprint,
            mineralState.copy(
              oreDelved = robotState.oreRobots + newMineralState.oreDelved,
              clayDelved = robotState.clayRobots + newMineralState.clayDelved,
              obsidianDelved = robotState.obsidianRobots + newMineralState.obsidianDelved,
              geodeDelved = robotState.geodeRobots + newMineralState.geodeDelved
            ),
            newRobotState
          )
        }

  object BlueprintState:
    def apply(blueprint: Blueprint): BlueprintState =
      BlueprintState(blueprint, mineralState = MineralState(), robotState = RobotState(oreRobots = 1L))

  case class Blueprint(num: Int, ore: OreRobot, clay: ClayRobot, obsidian: ObsidianRobot, geode: GeodeRobot):
    lazy val maxOreExpensePerMinute: Long      = ore.ore max clay.ore max obsidian.ore max geode.ore
    lazy val maxClayExpensePerMinute: Long     = obsidian.clay
    lazy val maxObsidianExpensePerMinute: Long = geode.obsidian

    def removeInferior(mineralStates: Set[MineralState]): Set[MineralState] =
      mineralStates.toList
        .combinations(2)
        .flatMap { case a :: b :: Nil =>
          a.allMax(b)
        }
        .toSet

    def optimize(states: Set[model.BlueprintState]): Set[model.BlueprintState] =
      states
        .groupBy(_.robotState)
        .flatMap {
          case (_, bs) if bs.size == 1 => Set(bs.head)
          case (rs, bs) =>
            val blueprint = bs.head.blueprint
            removeInferior(bs.map(_.mineralState)).map(ms => BlueprintState(blueprint, ms, rs))
        }
        .toList
        .sortBy(-_.arbritraryScore)
        .slice(0, 1000) // IDK anymore. Seems to reduce the space of states to a manageable size :shrug:
        .toSet

    def run(minutes: Int): BlueprintState =
      def optimizeGeodes(state: BlueprintState): BlueprintState =
        def rec(remaining: Int, queue: Set[BlueprintState], nextStates: Set[BlueprintState] = Set.empty): Set[BlueprintState] =
          if (remaining == 0) queue
          else if (queue.isEmpty) rec(remaining - 1, optimize(nextStates))
          else
            val (current, rest) = queue.head -> queue.tail
            val addNextStates   = current.minuteStep
            rec(remaining, rest, nextStates ++ addNextStates)
        val states = rec(minutes, Set(state))
        states.toList.maxBy(_.mineralState.geodeDelved)
      optimizeGeodes(BlueprintState(this))
}
