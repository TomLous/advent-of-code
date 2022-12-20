package aoc2020.day4

import aoc2020.day4.model.*
import zio.*
import zio.stream.*

object Solution {

  def parseLine(line: String): List[Input] = line.trim
    .split(" ")
    .flatMap {
      case s"byr:$byr"   => Some(BirthYear(byr.toInt))
      case s"iyr:$iyr"   => Some(IssueYear(iyr.toInt))
      case s"eyr:$eyr"   => Some(ExpirationYear(eyr.toInt))
      case s"hgt:${hgt}cm" => Some(Height(hgt.toInt, "cm"))
      case s"hgt:${hgt}in" => Some(Height(hgt.toInt, "in"))
      case s"hgt:${hgt}" => Some(Height(hgt.toInt, ""))
      case s"hcl:$hc"   => Some(HairColor(hc))
      case s"ecl:$ecl"   => Some(EyeColor(ecl))
      case s"pid:$pid"   => Some(PassportId(pid))
      case s"cid:$cid"   => Some(CountryId(cid.toInt))
      case ""            => None
      case e             => throw new IllegalArgumentException(s"Invalid input: $line ($e)")
    }
    .toList

  def parseInput(lineStream: ZStream[Any, Throwable, String]): ZIO[Any, Throwable, List[Passport]] = lineStream
    .map(parseLine)
    .splitOnChunk(Chunk(Nil))
    .map(c => Passport(c.toList.flatten))
    .runCollect
    .map(_.toList)

  def solvePart1(input: List[Passport]): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.count(_.isValidPart1))

  def solvePart2(input: List[Passport]): ZIO[Any, Throwable, BigInt] =
    ZIO.succeed(input.count(_.isValidPart2))

}
