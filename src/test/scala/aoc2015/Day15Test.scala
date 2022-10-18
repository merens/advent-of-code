package aoc2015

import aoc2015.Day15.Ingredient
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {
  test("score") {
    val butterscotch = Ingredient("Butterscotch", -1, -2, 6, 3, 8)
    val cinnamon = Ingredient("Cinnamon", 2, 3, -2, -1, 3)
    val teaspoons = Seq(44, 56)
    assert(Day15.calculateScore(Seq(butterscotch, cinnamon), teaspoons) == 62842880)
  }
}
