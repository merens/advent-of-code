package aoc2015

import aoc2015.Day05._
import org.scalatest.funsuite.AnyFunSuite

class Day05Test extends AnyFunSuite {
  test("3 vowels") {
    assert(atLeast3Vowels("abcdefghi"))
    assert(atLeast3Vowels("aaa"))
    assert(!atLeast3Vowels("aeb"))
  }

  test("dups") {
    assert(hasDouble("aeeiou"))
    assert(!hasDouble("aeb"))
  }

  test("naughty pairs") {
    assert(doesNotContainNaughtyPairs("aeeiou"))
    assert(!doesNotContainNaughtyPairs("abe"))
  }

  test("nice") {
    assert(isNice("ugknbfddgicrmopn"))
    assert(isNice("aaa"))
    assert(!isNice("jchzalrnumimnmhp"))
    assert(!isNice("haegwjzuvuyypxyu"))
    assert(!isNice("dvszwmarrgswjxmb"))
  }

  test("dups pair") {
    assert(hasDoublePair("xyxy"))
    assert(!hasDoublePair("aaa"))
  }

  test("nice 2") {
    assert(isNice2("qjhvhtzxzqqjkmpb"))
    assert(isNice2("xxyxx"))
    assert(!isNice2("aaa"))
    assert(!isNice2("uurcxstgmygtbstg"))
    assert(!isNice2("ieodomkazucvgmuy"))
  }
}
