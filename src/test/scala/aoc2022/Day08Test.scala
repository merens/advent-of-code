package aoc2022

import aoc2022.Day08._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import utils.Grid

class Day08Test extends AnyFunSuite {

  val INPUT: String = "30373\n25512\n65332\n33549\n35390"

  val FOREST: Grid[Int] = Vector(
    Vector(3, 0, 3, 7, 3),
    Vector(2, 5, 5, 1, 2),
    Vector(6, 5, 3, 3, 2),
    Vector(3, 3, 5, 4, 9),
    Vector(3, 5, 3, 9, 0)
  )

  test("parsing") {
    val actual = parseInput(INPUT)
    actual shouldBe FOREST
  }

  test("part 1 result") {
    val actual   = part1(FOREST)
    val expected = 21

    actual shouldBe expected
  }

  test("part 2 result") {
    val actual   = part2(FOREST)
    val expected = 8

    actual shouldBe expected
  }
}
