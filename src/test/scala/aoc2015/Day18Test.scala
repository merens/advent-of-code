package aoc2015

import aoc2015.Day18.{parseGrid, step}
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {
  test("step") {
    val input: String = ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
    val actual = step(Day18.parseGrid(input))
    val expected = "..##..\n..##.#\n...##.\n......\n#.....\n#.##.."

    assert(actual.equals(parseGrid(expected)))
  }
}
