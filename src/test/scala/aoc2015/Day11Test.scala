package aoc2015

import aoc2015.Day11.{ hasAtLeastTwoAdjacentDuplicate, hasConfusingLetters, hasStraightOfLetters, increaseString }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day11Test extends AnyFunSuite {
  test("straight") {
    assert(hasStraightOfLetters("cde"))
    assert(hasStraightOfLetters("abdef"))
    assert(!hasStraightOfLetters("abd"))
  }

  test("confusing") {
    assert(!hasConfusingLetters("abc"))
    assert(!hasConfusingLetters("abjjpk"))
    assert(hasConfusingLetters("dsapoj"))
  }

  test("double pairs") {
    assert(hasAtLeastTwoAdjacentDuplicate("aadcc"))
    assert(hasAtLeastTwoAdjacentDuplicate("kpdsahhaeii"))
    assert(!hasAtLeastTwoAdjacentDuplicate("dsapoj"))
  }

  test("increase string") {
    increaseString("a") shouldBe "b"
    increaseString("aa") shouldBe "ab"
    increaseString("az") shouldBe "ba"
    increaseString("aaz") shouldBe "aba"
  }

}
