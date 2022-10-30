package aoc2016

import org.scalatest.funsuite.AnyFunSuite

class Day09Test extends AnyFunSuite {
  test("decompressed length v1") {
    assert(Day09.decompressVersion1("ADVENT").equals(6))
    assert(Day09.decompressVersion1("A(1x5)BC").equals(7))
    assert(Day09.decompressVersion1("(3x3)XYZ").equals(9))
    assert(Day09.decompressVersion1("(6x1)(1x3)A").equals(6))
    assert(Day09.decompressVersion1("X(8x2)(3x3)ABCY").equals(18))
  }

  test("decompressed length v2") {
    assert(Day09.decompressVersion2("(3x3)XYZ").equals(9L))
    assert(Day09.decompressVersion2("X(8x2)(3x3)ABCY").equals(20L))
    assert(Day09.decompressVersion2("(27x12)(20x12)(13x14)(7x10)(1x12)A").equals(241920L))
    assert(Day09.decompressVersion2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN").equals(445L))
  }

}
