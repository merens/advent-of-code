package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day05Test extends AnyFunSuite {
  test("3 vowels") {
    assert(Day05.atLeast3Vowels("abcdefghi"))
    assert(Day05.atLeast3Vowels("aaa"))
    assert(!Day05.atLeast3Vowels("aeb"))
  }

  test("dups") {
    assert(Day05.hasDouble("aeeiou"))
    assert(!Day05.hasDouble("aeb"))
  }

  test("naughty pairs") {
    assert(Day05.doesNotContainNaughtyPairs("aeeiou"))
    assert(!Day05.doesNotContainNaughtyPairs("abe"))
  }

  test("nice") {
    assert(Day05.isNice("ugknbfddgicrmopn"))
    assert(Day05.isNice("aaa"))
    assert(!Day05.isNice("jchzalrnumimnmhp"))
    assert(!Day05.isNice("haegwjzuvuyypxyu"))
    assert(!Day05.isNice("dvszwmarrgswjxmb"))
  }

  test("dups pair") {
    assert(Day05.hasDoublePair("xyxy"))
    assert(!Day05.hasDoublePair("aaa"))
  }

  test("nice 2") {
    assert(Day05.isNice2("qjhvhtzxzqqjkmpb"))
    assert(Day05.isNice2("xxyxx"))
    assert(!Day05.isNice2("aaa"))
    assert(!Day05.isNice2("uurcxstgmygtbstg"))
    assert(!Day05.isNice2("ieodomkazucvgmuy"))
  }
}
