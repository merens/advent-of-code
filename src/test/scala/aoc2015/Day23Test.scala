package aoc2015

import aoc2015.Day23.{State, logic}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day23Test extends AnyFunSuite {
  test("test instruction increment") {
    val s           = State(Map("a" -> 0, "b" -> 0), 0)
    val instruction = "inc a"
    val expected    = State(Map("a" -> 1, "b" -> 0), 1)
    val actual      = logic(s, instruction)

    actual shouldBe expected
  }

  test("test instruction triple") {
    val s           = State(Map("a" -> 1, "b" -> 2), 3)
    val instruction = "tpl b"
    val expected    = State(Map("a" -> 1, "b" -> 6), 4)
    val actual      = logic(s, instruction)

    actual shouldBe expected
  }
  test("test instruction half") {
    val s           = State(Map("a" -> 1, "b" -> 2), 3)
    val instruction = "hlf b"
    val expected    = State(Map("a" -> 1, "b" -> 1), 4)
    val actual      = logic(s, instruction)

    actual shouldBe expected
  }

  test("test instruction forward jump") {
    val s           = State(Map("a" -> 1, "b" -> 2), 3)
    val instruction = "jmp +5"
    val expected    = State(Map("a" -> 1, "b" -> 2), 8)
    val actual      = logic(s, instruction)

    actual shouldBe expected
  }

  test("test instruction backwards jump") {
    val s           = State(Map("a" -> 1, "b" -> 2), 5)
    val instruction = "jmp -4"
    val expected    = State(Map("a" -> 1, "b" -> 2), 1)
    val actual      = logic(s, instruction)

    actual shouldBe expected
  }
}
