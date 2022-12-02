package aoc2022

import org.scalatest.funsuite.AnyFunSuite

class Day02Test extends AnyFunSuite {
  val INPUT: String = "A Y\nB X\nC Z"

  val moves: Seq[Seq[Char]] = Seq(
    Seq('A', 'Y'),
    Seq('B', 'X'),
    Seq('C', 'Z')
  )


  test("parsing") {
    assert(moves.equals(Day02.parseInput(INPUT)))
  }

  test("rock paper scissors") {
    assert(8.equals(Day02.rPSscore('A', 'Y')))
    assert(1.equals(Day02.rPSscore('B', 'X')))
    assert(6.equals(Day02.rPSscore('C', 'Z')))
  }

}
