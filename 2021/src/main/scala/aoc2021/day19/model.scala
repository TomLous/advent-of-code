package aoc2021.day19
import aoc2021.day5.model.LineSegment


object model {

  type Fingerprint = Long
  def beaconsToLineSegments(beacons: Set[Beacon]): Map[Fingerprint, LineSegment] = beacons.toList
      .combinations(2)
      .map { case List(a, b) =>
        val ls = LineSegment(a, b)
        ls.fingerprint -> ls
      }
      .toMap

  case class Point(x: Int, y: Int, z: Int=0):
    override def toString: String = s"$x,$y,$z"

    val positions: List[Int] = List(x, y, z)

    def manhattanDistance(other: Point): Int = math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)
    def rotate(rotation: Rotation):Point = rotation match
      case Rotation(xMod, yMod, zMod, Arrangement(xPos, yPos, zPos)) =>
        Point(
          x = positions(xPos) * xMod,
          y = positions(yPos) * yMod,
          z = positions(zPos) * zMod
        )
    def getTranslation(target: Point):Translation = Point(
      x = x - target.x,
      y = y - target.y,
      z = z - target.z
    )

    def translate(translation: Translation):Point = Point(
      x = x + translation.x,
      y = y + translation.y,
      z = z + translation.z
    )

    def transform(transformation: Transformation):Point = rotate(transformation.rotation).translate(transformation.translation)

  type ScannerPoint = Point
  type Beacon = Point
  type Direction = Point
  type Translation = Point

  case class Arrangement(xPos: Int, yPos: Int, zPos: Int)
  object Arrangement:
    def apply(list: List[Int]): Arrangement = Arrangement(list(0), list(1), list(2))
  case class Rotation(xMod: Int, yMod: Int, zMod: Int = 0, arrangement: Arrangement)

  object Rotation:
    lazy val allRotations:Set[Rotation] = (for{
      xMod <- List(1,-1)
      yMod <- List(1,-1)
      zMod <- List(1,-1)
      rotateMod <- List(0,1,2).permutations
    } yield Rotation(xMod,yMod,zMod, Arrangement(rotateMod))).toSet

  case class Transformation(rotation: Rotation, translation: Translation)

  case class LineSegment(a: Point, b: Point):
    lazy val dx:Int = a.x - b.x
    lazy val dy:Int = a.y - b.y
    lazy val dz:Int = a.z - b.z
    lazy val fingerprint: Fingerprint = dx.toLong * dx.toLong + dy.toLong * dy.toLong + dz.toLong * dz.toLong
    lazy val direction:Direction = Point(dx.compare(0), dy.compare(0), dz.compare(0))

    def rotate(rotation: Rotation): LineSegment = LineSegment(a.rotate(rotation), b.rotate(rotation))

    def getTranslation(target: LineSegment):Option[Translation] = {
      val transA = target.a.getTranslation(a)
      val transB = target.b.getTranslation(b)
      if(transA == transB) Some(transA) else None
    }

  case class Space(beacons: Set[Beacon], scanners: Set[ScannerPoint]):
    lazy val beaconLineSegments:  Map[Fingerprint, LineSegment] = beaconsToLineSegments(beacons)
    lazy val beaconFingerprints: Set[Fingerprint] = beaconLineSegments.keySet

    def orderCandidates(scanners: List[Scanner]):List[(Set[Fingerprint], Scanner)] =
      scanners
        .map(s => (s.beaconFingerprints.intersect(beaconFingerprints), s))
        .sortBy(-_._1.size)

    def transformScanner(scannerCandidate: (Set[Fingerprint], Scanner)): Option[Scanner] = scannerCandidate match {
      case (overlappingFingerprints, scanner) =>
        val matchingLineSegments = overlappingFingerprints.map(fingerprint => (beaconLineSegments(fingerprint), scanner.beaconLineSegments(fingerprint)))
        (for {
        (spaceLineSegment, scannerLineSegment) <- matchingLineSegments.toList
        rotation <- Rotation.allRotations.toList
        if scannerLineSegment.direction.rotate(rotation) == spaceLineSegment.direction
        rotatedScannerLineSegment = scannerLineSegment.rotate(rotation)
        translation <- rotatedScannerLineSegment.getTranslation(spaceLineSegment).toList
      } yield Transformation(rotation, translation))
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .toList
        .filter(_._2 > Scanner.minScannerOverlap)
        .sortBy(-_._2)
        .map(_._1)
        .headOption.map(scanner.transform)
    }

    def addScanner(scanner: Scanner): Space = Space(beacons ++ scanner.beacons, scanners + scanner.scannerPoint)

  object Space:

    def addScanners(scanners: List[Scanner], space: Space): Space =
    {
      if scanners.isEmpty then space
      else
        val scannerCandidates: List[(Set[Fingerprint], Scanner)] = space.orderCandidates(scanners)
        val remaining:List[Scanner] = scannerCandidates.tail.map(_._2)
        val scannerCandidate:(Set[Fingerprint], Scanner) = scannerCandidates.head
        val orientedScanner:Option[Scanner] = space.transformScanner(scannerCandidate)
        orientedScanner match
          case None if remaining.isEmpty =>
            throw new Exception("No more scanners could be placed")
          case None =>
            val unmatchedScanner = scannerCandidate._2
            println(s"WARNING: no match found for scanner ${unmatchedScanner.num}, trying another one first")
            addScanners(List(unmatchedScanner), addScanners(remaining, space))
          case Some(scanner) =>
            addScanners(remaining, space.addScanner(scanner))
    }


    def init(scanners: List[Scanner]): Space =
      val initSpace = Space(scanners.head.beacons.toSet, Set(scanners.head.scannerPoint))
      addScanners(scanners.tail, initSpace): Space


  case class Scanner(num: Int, beacons: List[Beacon], scannerPoint: ScannerPoint= Point(0,0,0)):
    lazy val beaconLineSegments: Map[Fingerprint, LineSegment] = beaconsToLineSegments(beacons.toSet)
    lazy val beaconFingerprints: Set[Fingerprint] = beaconLineSegments.keySet

    def transform(transformation: Transformation): Scanner = Scanner(num, beacons.map(_.transform(transformation)), scannerPoint.transform(transformation))







  object Scanner:
    val maxRange:Int = 1000
    val minScannerOverlap:Int = 3

    def apply(header: String, beacons: List[String]): Scanner =
      Scanner(
        header match {
          case s"--- scanner $scannerNum ---" => scannerNum.toInt
          case _ => throw new IllegalArgumentException(s"Invalid header: $header")
        },
        beacons.map(_.split(",").map(_.trim.toInt).toList match
          case x :: y :: Nil => Point(x, y)
          case x :: y :: z :: Nil => Point(x, y, z)
          case _ => throw new IllegalArgumentException(s"Invalid beacon: $beacons")
        )
      )



}
