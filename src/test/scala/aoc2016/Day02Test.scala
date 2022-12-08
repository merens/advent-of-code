package aoc2016

import aoc2016.Day02.{ grid, linesFunction, nextPos }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import utils.Pos

class Day02Test extends AnyFunSuite {
  test("instructions") {
    nextPos(Pos(1, 1), 'U') shouldBe Pos(0, 1)
    nextPos(Pos(1, 1), 'L') shouldBe Pos(1, 0)
    nextPos(Pos(0, 0), 'L') shouldBe Pos(0, 0)
    nextPos(Pos(1, 1), 'D') shouldBe Pos(2, 1)
    nextPos(Pos(1, 1), 'R') shouldBe Pos(1, 2)
  }

  test("line instruction") {
    linesFunction(Seq("ULL"), nextPos, grid) shouldBe Seq(1)
    linesFunction(Seq("RRDDD", "LURDL", "UUUUD"), nextPos, grid) shouldBe Seq(9, 8, 5)
  }

}
