package aoc2022

import org.scalatest.funsuite.AnyFunSuite

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
    assert(COMPARTMENTS.equals(Day03.parseInput(INPUT)))
  }

  test("find duplicates in compartments") {
    assert('p'.equals(Day03.findDuplicateCompartments(COMPARTMENTS.head)))
    assert('L'.equals(Day03.findDuplicateCompartments(COMPARTMENTS(1))))
    assert('P'.equals(Day03.findDuplicateCompartments(COMPARTMENTS(2))))
    assert('v'.equals(Day03.findDuplicateCompartments(COMPARTMENTS(3))))
    assert('t'.equals(Day03.findDuplicateCompartments(COMPARTMENTS(4))))
    assert('s'.equals(Day03.findDuplicateCompartments(COMPARTMENTS.last)))
  }

  test("find duplicates in a group of three compartments") {
    assert('r'.equals(Day03.findDuplicateGroup(COMPARTMENTS.take(3))))
    assert('Z'.equals(Day03.findDuplicateGroup(COMPARTMENTS.takeRight(3))))
  }

  test("calculate priority") {
    assert(16.equals(Day03.calcPriority('p')))
    assert(38.equals(Day03.calcPriority('L')))
    assert(42.equals(Day03.calcPriority('P')))
    assert(22.equals(Day03.calcPriority('v')))
    assert(20.equals(Day03.calcPriority('t')))
    assert(19.equals(Day03.calcPriority('s')))

    assert(18.equals(Day03.calcPriority('r')))
    assert(52.equals(Day03.calcPriority('Z')))
  }

  test("totals") {
    assert(157.equals(Day03.part1(COMPARTMENTS)))
    assert(70.equals(Day03.part2(COMPARTMENTS)))
  }

}
