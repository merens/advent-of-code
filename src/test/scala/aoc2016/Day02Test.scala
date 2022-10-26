package aoc2016

import org.scalatest.funsuite.AnyFunSuite
import utils.Pos

class Day02Test extends AnyFunSuite {
  test("instructions") {
    assert(Day02.nextPos(Pos(1, 1), 'U') == Pos(1, 0))
    assert(Day02.nextPos(Pos(1, 1), 'L') == Pos(0, 1))
    assert(Day02.nextPos(Pos(0, 0), 'L') == Pos(0, 0))
    assert(Day02.nextPos(Pos(1, 1), 'D') == Pos(1, 2))
    assert(Day02.nextPos(Pos(1, 1), 'R') == Pos(2, 1))
  }

  test("line instruction") {
    assert(Day02.linesFunction(Seq("ULL"), Day02.nextPos, Day02.grid) == Seq(1))
    assert(Day02.linesFunction(Seq("RRDDD", "LURDL", "UUUUD"), Day02.nextPos, Day02.grid) == Seq(9, 8, 5))
  }

}
