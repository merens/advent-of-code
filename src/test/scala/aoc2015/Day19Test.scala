package aoc2015

import aoc2015.Day19.{ allReplacements, replace }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day19Test extends AnyFunSuite {
  test("replace one rule") {
    val input: String = "HOH"
    val map           = Map("H" -> Seq("HO", "OH"), "O" -> Seq("HH"))
    val expected      = Seq("HOOH", "HOHO")
    val actual        = replace(input, "H", map("H").head)
    println(actual)
    actual shouldBe expected
  }

  test("all replacements") {
    val input: String = "HOH"
    val map           = Map("H" -> Seq("HO", "OH"), "O" -> Seq("HH"))
    val expected      = Seq("HOOH", "HOHO", "OHOH", "HOOH", "HHHH")
    val actual        = allReplacements(input, map)
    println(actual)
    actual shouldBe expected
  }

  test("count distinct replacements") {
    val input: String = "HOH"
    val map           = Map("H" -> Seq("HO", "OH"), "O" -> Seq("HH"))
    val expected      = 4
    val actual        = allReplacements(input, map).distinct.size
    println(actual)
    actual shouldBe expected
  }
}
