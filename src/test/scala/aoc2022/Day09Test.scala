package aoc2022

import aoc2022.Day09._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import utils.{ Grid, Pos }
import utils.GridImplicits._

class Day09Test extends AnyFunSuite {

  val INPUT: String = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

  test("detach head") {
    val headPos: Pos   = Pos(1, 3)
    val tailPos: Pos   = Pos(1, 1)
    val movedDirection = "R"

    val actual   = updateTail(headPos, tailPos, movedDirection)
    val expected = Pos(1, 2)
    actual shouldBe expected
  }

  test("detach head 2") {
    val headPos: Pos   = Pos(3, 1)
    val tailPos: Pos   = Pos(1, 1)
    val movedDirection = "D"

    val actual   = updateTail(headPos, tailPos, movedDirection)
    val expected = Pos(2, 1)
    actual shouldBe expected
  }

  test("part 1 result") {
    val grid: Grid[Boolean]  = Vector.fill(5, 6)(false).updateGrid(Pos(4, 0), true)
    val startingHeadPos: Pos = Pos(4, 0)
    val startingTailPos: Pos = startingHeadPos
    val actual               = part1(grid, parseInput(INPUT), startingHeadPos, startingTailPos)
    val expected             = 13

    actual shouldBe expected
  }
}
