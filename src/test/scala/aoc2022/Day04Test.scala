package aoc2022

import aoc2022.Day04._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

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
    SECTIONS shouldBe parseInput(INPUT)
  }

  test("check concentric sections") {
    assert(!sectionsAreConcentric(2, 4, 6, 8))
    assert(!sectionsAreConcentric(2, 3, 4, 5))
    assert(!sectionsAreConcentric(5, 7, 7, 9))
    assert(sectionsAreConcentric(2, 8, 3, 7))
    assert(sectionsAreConcentric(6, 6, 4, 6))
    assert(!sectionsAreConcentric(2, 6, 4, 8))
  }

  test("total part 1") {
    part1(SECTIONS) shouldBe 2
  }

  test("count overlap sections") {
    assert(!sectionAreOverlapping(2, 4, 6, 8))
    assert(!sectionAreOverlapping(2, 3, 4, 5))
    assert(sectionAreOverlapping(5, 7, 7, 9))
    assert(sectionAreOverlapping(2, 8, 3, 7))
    assert(sectionAreOverlapping(6, 6, 4, 6))
    assert(sectionAreOverlapping(2, 6, 4, 8))
  }

  test("total part 2") {
    part2(SECTIONS) shouldBe 4
  }

}
