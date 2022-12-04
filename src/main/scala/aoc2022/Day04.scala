package aoc2022

import java.security.MessageDigest
import scala.util.matching.Regex

object Day04 {

  val sectionsRegex: Regex = """(\d+)-(\d+),(\d+)-(\d+)""".r

  def sectionsAreConcentric(a: Int, b: Int, c: Int, d: Int): Boolean =
    (a <= c && b >= d && (b - a) > (d - c)) ||
      (c <= a && d >= b && (d - c) >= (b - a))
  // maybe it could work something with sets, like comparing intersection and union
  // found this: https://www.reddit.com/r/adventofcode/comments/zc0zta/2022_day_4_solutions/iyua3ty/

  def sectionAreOverlapping(a: Int, b: Int, c: Int, d: Int): Boolean = {
    (a to b) intersect (c to d)
  }.nonEmpty

  def part1(sections: Seq[String]):Int = sections.map{
    case sectionsRegex(a, b, c, d) => sectionsAreConcentric(a.toInt, b.toInt, c.toInt, d.toInt)
    case e => throw new IllegalArgumentException(s"error parsing section: $e")
  }.count(_ == true)

  def part2(sections: Seq[String]):Int = sections.map{
    case sectionsRegex(a, b, c, d) => sectionAreOverlapping(a.toInt, b.toInt, c.toInt, d.toInt)
    case e => throw new IllegalArgumentException(s"error parsing section: $e")
  }.count(_ == true)

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day04.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(part1(parseInput(input)))
    println(part2(parseInput(input)))
  }
}


