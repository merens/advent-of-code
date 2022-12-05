package aoc2022

import aoc2022.Day03._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day03Test extends AnyFunSuite {

  val INPUT: String =
    "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

  val COMPARTMENTS: Seq[String] = Seq(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )

  test("parsing") {
    parseInput(INPUT) shouldBe COMPARTMENTS
  }

  test("find duplicates in compartments") {
    findDuplicateCompartments(COMPARTMENTS.head) shouldBe 'p'
    findDuplicateCompartments(COMPARTMENTS(1)) shouldBe 'L'
    findDuplicateCompartments(COMPARTMENTS(2)) shouldBe 'P'
    findDuplicateCompartments(COMPARTMENTS(3)) shouldBe 'v'
    findDuplicateCompartments(COMPARTMENTS(4)) shouldBe 't'
    findDuplicateCompartments(COMPARTMENTS.last) shouldBe 's'
  }

  test("find duplicates in a group of three compartments") {
    findDuplicateGroup(COMPARTMENTS.take(3)) shouldBe 'r'
    findDuplicateGroup(COMPARTMENTS.takeRight(3)) shouldBe 'Z'
  }

  test("calculate priority") {
    calcPriority('p') shouldBe 16
    calcPriority('L') shouldBe 38
    calcPriority('P') shouldBe 42
    calcPriority('v') shouldBe 22
    calcPriority('t') shouldBe 20
    calcPriority('s') shouldBe 19

    calcPriority('r') shouldBe 18
    calcPriority('Z') shouldBe 52
  }

  test("totals") {
    part1(COMPARTMENTS) shouldBe 157
    part2(COMPARTMENTS) shouldBe 70
  }

}
