package aoc2022.day20

import scala.collection.mutable.ListBuffer

object model {
  case class Decrypt(input: List[Int], key: Long = 1L, mixin: Int = 1):
    lazy val inputMapped: List[(Long, Int)] = input.map(_.toLong * key).zipWithIndex

    lazy val decryptedMapped: List[(Long, Int)] = (0 until mixin)
      .foldLeft(inputMapped.toBuffer) { case (premixed, _) =>
        inputMapped.foldLeft(premixed) { case (newBuffer, tuple) =>
          val move         = tuple._1
          val currentIndex = newBuffer.indexOf(tuple)
          newBuffer.remove(currentIndex)
          val newIndex  = (currentIndex + move) % newBuffer.size
          val realIndex = if newIndex < 0 then newIndex + newBuffer.size else newIndex
          newBuffer.insert(realIndex.toInt, tuple)
          newBuffer
        }
      }
      .toList

    lazy val decrypted: List[Long] = decryptedMapped.map(_._1)

    def groveCoordinates(nums: List[Int]): List[Long] =
      val index0 = decrypted.indexOf(0L)
      def getNumAt(index: Int): Long =
        val newIndex = (index + index0) % decrypted.length
        decrypted(newIndex.toInt)
      nums.map(getNumAt)

}
