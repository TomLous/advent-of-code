3 * 3 * 3



(for{
  r1 <- (1 to 3)
  r2 <- (1 to 3)
  r3 <- (1 to 3)
} yield r1 + r2 + r3).groupMapReduce(identity)(_ => 1)(_ + _)