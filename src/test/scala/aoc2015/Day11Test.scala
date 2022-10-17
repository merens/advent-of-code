package aoc2015

import org.scalatest.funsuite.AnyFunSuite
class Day11Test extends AnyFunSuite {
  test("straight"){
    assert(Day11.hasStraightOfLetters("cde"))
    assert(Day11.hasStraightOfLetters("abdef"))
    assert(!Day11.hasStraightOfLetters("abd"))
  }

  test("confusing"){
    assert(!Day11.hasConfusingLetters("abc"))
    assert(!Day11.hasConfusingLetters("abjjpk"))
    assert(Day11.hasConfusingLetters("dsapoj"))
  }

  test("double pairs"){
    assert(Day11.hasAtLeastTwoAdjacentDuplicate("aadcc"))
    assert(Day11.hasAtLeastTwoAdjacentDuplicate("kpdsahhaeii"))
    assert(!Day11.hasAtLeastTwoAdjacentDuplicate("dsapoj"))
  }

  test("increase string"){
    assert(Day11.increaseString("a").equals("b"))
    assert(Day11.increaseString("aa").equals("ab"))
    assert(Day11.increaseString("az").equals("ba"))
    assert(Day11.increaseString("aabz").equals("aba"))
  }

}
