package aoc2022.day7

import model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): Input =
    line match
      case s"$$ $command" => command match
        case s"cd $dir" => Cd(dir)
        case "ls" => Ls
      case s"dir $name" => Dir(name)
      case s"$size $name" => File(name, size.toInt)
  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, Dir] =
    lineStream
      .map(parseLine)
      .runFold(FSStack())(FSStack.traverseInput)
      .map(FSStack.toRootDir)

  def solvePart1(root: Dir): ZIO[Any, Throwable, Long] =
    ZIO.succeed(root.filterDirs(_.cumSize < 100000).map(_.cumSize).sum)

  def solvePart2(root: Dir): ZIO[Any, Throwable, Long] =
    val diskSize = 70000000L
    val spaceNeeded = 30000000L
    val sumSizeInUse = root.cumSize
    val sumSizeFree = diskSize - sumSizeInUse
    val sizeToDelete = spaceNeeded - sumSizeFree

    val potentialDirs = root.filterDirs(_.cumSize >= sizeToDelete)
    val dirSize = potentialDirs.map(_.cumSize).min
    
    ZIO.succeed(dirSize)

}
