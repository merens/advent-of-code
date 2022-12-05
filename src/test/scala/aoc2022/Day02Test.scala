package aoc2022

import aoc2022.Day02.{ parseInput, rPSscore, rPSscore2 }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day02Test extends AnyFunSuite {
  val INPUT: String = "A Y\nB X\nC Z"

  val MOVES: Seq[Seq[Char]] = Seq(
    Seq('A', 'Y'),
    Seq('B', 'X'),
    Seq('C', 'Z')
  )

  test("parsing") {
    parseInput(INPUT) shouldBe MOVES
  }

  test("rock paper scissors") {
    rPSscore('A', 'Y') shouldBe 8
    rPSscore('B', 'X') shouldBe 1
    rPSscore('C', 'Z') shouldBe 6
  }

  test("rock paper scissors refactor") {
    rPSscore2('A', 'Y') shouldBe 8
    rPSscore2('B', 'X') shouldBe 1
    rPSscore2('C', 'Z') shouldBe 6
  }

}
