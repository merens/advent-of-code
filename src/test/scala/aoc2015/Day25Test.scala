package aoc2015

import aoc2015.Day25.toIndex
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {
  test("test to index") {
    assert(toIndex(4, 2) == 12)
    assert(toIndex(1, 1) == 1)
    assert(toIndex(3, 3) == 13)
    assert(toIndex(1, 6) == 21)
  }

}
