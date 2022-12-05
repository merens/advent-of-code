package aoc2015

import aoc2015.Day25.toIndex
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day25Test extends AnyFunSuite {
  test("test to index") {
    toIndex(4, 2) shouldBe 12
    toIndex(1, 1) shouldBe 1
    toIndex(3, 3) shouldBe 13
    toIndex(1, 6) shouldBe 21
  }

}
