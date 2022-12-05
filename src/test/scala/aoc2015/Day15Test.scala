package aoc2015

import aoc2015.Day15.{ calculateScore, Ingredient }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day15Test extends AnyFunSuite {
  test("score") {
    val butterscotch = Ingredient("Butterscotch", -1, -2, 6, 3, 8)
    val cinnamon     = Ingredient("Cinnamon", 2, 3, -2, -1, 3)
    val teaspoons    = Seq(44, 56)
    calculateScore(Seq(butterscotch, cinnamon), teaspoons) shouldBe 62842880
  }
}
