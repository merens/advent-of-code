package aoc2022

import org.scalatest.funsuite.AnyFunSuite

class Day04Test extends AnyFunSuite {

  val INPUT: String =
    "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

  val SECTIONS: Seq[String] = Seq(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  test("parsing") {
    assert(SECTIONS.equals(Day04.parseInput(INPUT)))
  }

  test("check concentric sections") {
    assert(!Day04.sectionsAreConcentric(2, 4, 6, 8))
    assert(!Day04.sectionsAreConcentric(2, 3, 4, 5))
    assert(!Day04.sectionsAreConcentric(5, 7, 7, 9))
    assert(Day04.sectionsAreConcentric(2, 8, 3, 7))
    assert(Day04.sectionsAreConcentric(6, 6, 4, 6))
    assert(!Day04.sectionsAreConcentric(2, 6, 4, 8))
  }

  test("total part 1") {
    assert(2.equals(Day04.part1(SECTIONS)))
  }

  test("count overlap sections") {
    assert(!Day04.sectionAreOverlapping(2, 4, 6, 8))
    assert(!Day04.sectionAreOverlapping(2, 3, 4, 5))
    assert(Day04.sectionAreOverlapping(5, 7, 7, 9))
    assert(Day04.sectionAreOverlapping(2, 8, 3, 7))
    assert(Day04.sectionAreOverlapping(6, 6, 4, 6))
    assert(Day04.sectionAreOverlapping(2, 6, 4, 8))
  }

  test("total part 2") {
    assert(4.equals(Day04.part2(SECTIONS)))
  }

}
