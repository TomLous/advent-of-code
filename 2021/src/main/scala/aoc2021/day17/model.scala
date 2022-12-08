package aoc2021.day17

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.DefaultGraphImpl

object model {

  case class Target(xLeft: Int, xRight: Int, yTop: Int, yBottom: Int):
    def overshoot(x: Int, y: Int): Boolean =
      y < yBottom || x > xRight
    def hit(x: Int, y: Int): Boolean =
      (y >= yBottom && y <= yTop) && (x >= xLeft && x <= xRight)

  case class Ocean(target: Target):
    def checkHit[T, U](xSpeed: Int, ySpeed: Int, f: (Int, Int, Int, Int) => T, f2: List[T] => U): (Boolean, U) =
      def step(x: Int, y: Int, _xSpeed: Int, _ySpeed: Int, vals: List[T]): (Boolean, List[T]) =
        val newVals = vals :+ f(x, y, xSpeed, ySpeed)
        if target.overshoot(x, y) then (false, newVals)
        else if target.hit(x, y) then (true, newVals)
        else
          val _newXSpeed = if _xSpeed == 0 then 0 else _xSpeed - 1
          val _newYSpeed = _ySpeed - 1
          step(x + _xSpeed, y + _ySpeed, _newXSpeed, _newYSpeed, newVals)
      val r = step(0, 0, xSpeed, ySpeed, Nil)
      (r._1, f2(r._2))


    def checkYSpeeds[T, U](f: (Int, Int, Int, Int) => T, f2: List[T] => U)(xSpeed: Int): List[U] =
      val maxInitYSpeed = math.abs(target.yBottom) // don't know if this is true though
      (0 to maxInitYSpeed).map(ySpeed => checkHit(xSpeed, ySpeed, f, f2)).toList
      def loop(curInitYSpeed: Int, keepLoop: Boolean = true, solutions: List[U] = Nil): List[U] =
        val (hit, fRes) = checkHit(xSpeed, curInitYSpeed, f, f2)
        if hit then loop(curInitYSpeed + 1, curInitYSpeed < maxInitYSpeed, solutions :+ fRes)
        else if keepLoop then loop(curInitYSpeed + 1, curInitYSpeed < maxInitYSpeed, solutions)
        else solutions
      loop(target.yBottom)

    lazy val maxYforTarget: Int =
      val maxXSpeed = target.xRight

      val speeds = maxXSpeed until 0 by -1 flatMap checkYSpeeds[Int, Int]((_, y, _, _) => y, yList => yList.max)

      speeds.max

    lazy val distinctInitSpeeds: Int =
      val maxXSpeed = target.xRight

      val initStates = maxXSpeed until 0 by -1 flatMap checkYSpeeds[(Int, Int), List[(Int, Int)]]((_, _, xInitSpeed, yInitSpeed) => (xInitSpeed,yInitSpeed), t => t )

      initStates.flatten.distinct.size

}
