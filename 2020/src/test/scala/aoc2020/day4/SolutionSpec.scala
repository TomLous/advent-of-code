package aoc2020.day4

import aoc2020.day4.Solution
import zio.*
import zio.stream.*
import zio.test.*

object SolutionSpec extends ZIOSpecDefault {

  val prefix = "AoC 2020 - Day 4"

  private val sampleData1: String =
    """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in""".stripMargin

  private val sampleData2: String =
    """eyr:1972 cid:100
      |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
      |
      |iyr:2019
      |hcl:#602927 eyr:1967 hgt:170cm
      |ecl:grn pid:012533040 byr:1946
      |
      |hcl:dab227 iyr:2012
      |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
      |
      |hgt:59cm ecl:zzz
      |eyr:2038 hcl:74454a iyr:2023
      |pid:3556412378 byr:2007""".stripMargin

  private val sampleData3: String =
    """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
      |hcl:#623a2f
      |
      |eyr:2029 ecl:blu cid:129 byr:1989
      |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
      |
      |hcl:#888785
      |hgt:164cm byr:2001 iyr:2015 cid:88
      |pid:545766238 ecl:hzl
      |eyr:2022
      |
      |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".stripMargin

  def spec: Spec[Any, Throwable] = suite(s"$prefix Solution")(
    test("part 1 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 2

      for {
        data    <- Solution.parseInput(input)
        _       <- Console.printLine(data)
        output1 <- Solution.solvePart1(data)
      } yield assertTrue(output1 == expectedOutput)
    },
    test("part 2 - example 1") {
      val input                  = ZStream.fromIterable(sampleData1.split("\n"))
      val expectedOutput: BigInt = 2

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    },
    test("part 2 - example 2") {
      val input                  = ZStream.fromIterable(sampleData2.split("\n"))
      val expectedOutput: BigInt = 0

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    },
    test("part 2 - example 3") {
      val input                  = ZStream.fromIterable(sampleData3.split("\n"))
      val expectedOutput: BigInt = 4

      for {
        data    <- Solution.parseInput(input)
        output2 <- Solution.solvePart2(data)
      } yield assertTrue(output2 == expectedOutput)
    },
  )
}
