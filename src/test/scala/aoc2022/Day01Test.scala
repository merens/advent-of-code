package aoc2022

import aoc2022.Day01.{ max, parseInput, top3 }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day01Test extends AnyFunSuite {
  val INPUT: String  = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
  val INPUT2: String = "1000\r\n2000\r\n3000\r\n\r\n4000\r\n\r\n5000\r\n6000\r\n\r\n7000\r\n8000\r\n9000\r\n\r\n10000"
  val INPUT3: String = "1000\r2000\r3000\r\r4000\r\r5000\r6000\r\r7000\r8000\r9000\r\r10000"

  val ELVES: Seq[Seq[Int]] = Seq(
    Seq(1000, 2000, 3000),
    Seq(4000),
    Seq(5000, 6000),
    Seq(7000, 8000, 9000),
    Seq(10000)
  )

  test("parsing") {
    parseInput(INPUT) shouldBe ELVES
  }

  test("parse different new line types") {
    parseInput(INPUT2) shouldBe ELVES
    parseInput(INPUT3) shouldBe ELVES
  }

  test("find max") {
    val expected = Seq(7000, 8000, 9000).sum
    max(ELVES) shouldBe expected
    max(ELVES) shouldBe 24000
  }

  test("sum top three max") {
    val expected = Seq(5000, 6000, 7000, 8000, 9000, 10000).sum
    top3(ELVES) shouldBe expected
    top3(ELVES) shouldBe 45000
  }
}
