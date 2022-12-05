package aoc2016

import aoc2016.Day09.{ decompressVersion1, decompressVersion2 }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day09Test extends AnyFunSuite {
  test("decompressed length v1") {
    decompressVersion1("ADVENT") shouldBe 6
    decompressVersion1("A(1x5)BC") shouldBe 7
    decompressVersion1("(3x3)XYZ") shouldBe 9
    decompressVersion1("(6x1)(1x3)A") shouldBe 6
    decompressVersion1("X(8x2)(3x3)ABCY") shouldBe 18
  }

  test("decompressed length v2") {
    decompressVersion2("(3x3)XYZ") shouldBe 9L
    decompressVersion2("X(8x2)(3x3)ABCY") shouldBe 20L
    decompressVersion2("(27x12)(20x12)(13x14)(7x10)(1x12)A") shouldBe 241920L
    decompressVersion2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") shouldBe 445L
  }

}
