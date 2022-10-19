package aoc2015

import aoc2015.Day16.Sue
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {
  test("parse Sue") {
    val actual   = Day16.parseInput("Sue 1: children: 1, cars: 8, vizslas: 7")
    val expected = Sue(1, Map("children" -> 1, "cars" -> 8, "vizslas" -> 7))
    assert(actual.head.equals(expected))
  }
}
