package aoc2022

import aoc2022.Day06._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{contain, have}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day06Test extends AnyFunSuite {

  val INPUTS: Seq[String] = Seq(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )

  test("part 1 result") {
    val actual   = INPUTS.map(part1)
    val expected = Seq(7, 5, 6, 10, 11)

    actual should contain theSameElementsInOrderAs expected
  }

  test("part 2 result") {
    val actual   = INPUTS.map(part2)
    val expected = Seq(19, 23, 23, 29, 26)

    actual should contain theSameElementsInOrderAs expected
  }

}
