case class Point(x: Int, y: Int, z:Int):
  val positions = List(x, y, z)

val rotations = for{
  xMod <- List(1,-1)
  yMod <- List(1,-1)
  zMod <- List(1,-1)
  rotateMod <- List(0,1,2).permutations
} yield (xMod,yMod,zMod, rotateMod)

val p = Point(10, 20, 30)

val res = rotations.map{
  case (x, y, z, perm) => 
    Point(
      p.positions(perm(0)) * x,
      p.positions(perm(1)) * y,
      p.positions(perm(2)) * z
    )
}

println(res.toSet.size)
res.foreach(println)