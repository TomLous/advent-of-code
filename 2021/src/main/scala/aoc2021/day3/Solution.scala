package aoc2021.day3

import aoc2021.day2.model.Position
import aoc2021.day3.model.*
import zio.*
import zio.stream.ZStream

object Solution {


  def createBinaryTree(valueStream: ZStream[Any, Throwable, List[Int]]): ZIO[Any, Throwable, Branch] = valueStream.runFold(Branch()) {
    case (branch, binaryList) =>
      branch.propagate(binaryList)
  }

  def calculatePowerConsumption(valueStream: ZStream[Any, Throwable, List[Int]]): ZIO[Any, Throwable, PowerConsumption] = valueStream
    .runFold(BinaryTracker(Nil, 0L)) { case (BinaryTracker(sums, size), binaryList) =>
      BinaryTracker(binaryList.zipAll(sums, 0, 0).map(_ + _), size + 1)
    }
    .map(_.powerConsumption)




  def calculateLifeSupport(branch: Branch):ZIO[Any, Throwable, LifeSupport] = ZIO.succeed(branch.lifeSupport)



}
