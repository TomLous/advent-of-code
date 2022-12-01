package aoc2021.day19
import aoc2021.day5.model.LineSegment


object model {

  trait Point:
    def x: Int
    def y: Int
    def z: Int

  case class ScannerPoint(override val x: Int, override val y: Int, override val z: Int = 0) extends Point
  case class Beacon(override val x: Int, override val y: Int, override val z: Int = 0) extends Point

  case class LineSegment(a: Point, b: Point):
    lazy val dx:Int = a.x - b.x
    lazy val dy:Int = a.y - b.y
    lazy val dz:Int = a.z - b.z
    lazy val fingerprint: Double = math.sqrt(dx * dx + dy * dy + dz * dz)


  case class Space(points: List[Point]):
    lazy val beacons: List[Beacon] = points.collect { case b: Beacon => b }

    def scoreCandidates(scanners: List[Scanner]):List[(Int, Scanner)] = ???

    def matchScanner(scanner: Scanner): Option[Scanner] = ???
    def addScanner(scanner: Scanner): Space = ???

  object Space:
    def addScanners(scanners: List[Scanner], space: Space): Space = ???
//    {
//      if scanners.isEmpty then space
//      else
//        val scannerCandidates: List[(Int, Scanner)] = space.scoreCandidates(scanners)
//        val orientedScanner = space.matchScanner(scannerCandidates.head._2)
//        orientedScanner match
//          case None => throw new Exception(s"Can't add scanner ${orientedScanner._2.num} to space")
//          case Some(scanner) => addScanners(scannerCandidates.tail.map(_._2), space.addScanner(scanner))
//    }


    def init(scanners: List[Scanner]): Space =
      val initSpace = Space(scanners.head.scannerPoint :: scanners.head.beacons)
      addScanners(scanners.tail, initSpace): Space


  case class Scanner(num: Int, beacons: List[Beacon]):
    val scannerPoint:ScannerPoint = ScannerPoint(0,0,0)
    lazy val beaconLineSegments: List[LineSegment] = beacons
      .combinations(2)
      .map{
        case List(a,b) => LineSegment(a,b)
      }
      .toList






  object Scanner:
    val maxRange:Int = 1000

    def apply(header: String, beacons: List[String]): Scanner =
      Scanner(
        header match {
          case s"--- scanner $scannerNum ---" => scannerNum.toInt
          case _ => throw new IllegalArgumentException(s"Invalid header: $header")
        },
        beacons.map(_.split(",").map(_.trim.toInt).toList match
          case x :: y :: Nil => Beacon(x, y)
          case x :: y :: z :: Nil => Beacon(x, y, z)
          case _ => throw new IllegalArgumentException(s"Invalid beacon: $beacons")
        )
      )



}
