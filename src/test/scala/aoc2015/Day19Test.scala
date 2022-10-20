package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {
  test("replace one rule") {
    val input: String = "HOH"
    val map           = Map("H" -> Seq("HO", "OH"), "O" -> Seq("HH"))
    val expected      = Seq("HOOH", "HOHO")
    val actual        = Day19.generate(input, "H", map("H").head)
    println(actual)
    assert(actual.equals(expected))
  }

  test("all replacements") {
    val input: String = "HOH"
    val map           = Map("H" -> Seq("HO", "OH"), "O" -> Seq("HH"))
    val expected      = Seq("HOOH", "HOHO", "OHOH", "HOOH", "HHHH")
    val actual        = Day19.allReplacements(input, map)
    println(actual)
    assert(actual.equals(expected))
  }

  test("count distinct replacements") {
    val input: String = "HOH"
    val map           = Map("H" -> Seq("HO", "OH"), "O" -> Seq("HH"))
    val expected      = 4
    val actual        = Day19.allReplacements(input, map).distinct.size
    println(actual)
    assert(actual.equals(expected))
  }
}
