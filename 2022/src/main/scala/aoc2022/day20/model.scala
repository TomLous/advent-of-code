package aoc2022.day20

import scala.collection.mutable.ListBuffer

object model {
  case class Decrypt(input: List[BigInt], key: BigInt = 1, mixin: Int = 1):
    lazy val inputMapped: List[(BigInt, Int)] = input.map(_ * key).zipWithIndex

    lazy val decryptedMapped: List[(BigInt, Int)] = (0 until mixin)
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

    lazy val decrypted: List[BigInt] = decryptedMapped.map(_._1)

    def groveCoordinates(nums: List[Int]): List[BigInt] =
      val index0 = decrypted.indexOf(0)
      def getNumAt(index: Int): BigInt =
        val newIndex = (index + index0) % decrypted.length
        decrypted(newIndex)
      nums.map(getNumAt)

}
