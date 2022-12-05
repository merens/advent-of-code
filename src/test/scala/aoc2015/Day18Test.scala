package aoc2015

import aoc2015.Day18.{ parseGrid, step }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day18Test extends AnyFunSuite {
  test("step") {
    val input: String = ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
    val actual        = step(parseGrid(input))
    val expected      = "..##..\n..##.#\n...##.\n......\n#.....\n#.##.."

    actual shouldBe parseGrid(expected)
  }
}
