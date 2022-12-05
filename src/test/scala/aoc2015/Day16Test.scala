package aoc2015

import aoc2015.Day16.{ parseInput, Sue }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day16Test extends AnyFunSuite {
  test("parse Sue") {
    val actual   = parseInput("Sue 1: children: 1, cars: 8, vizslas: 7")
    val expected = Sue(1, Map("children" -> 1, "cars" -> 8, "vizslas" -> 7))
    actual.head shouldBe expected
  }
}
