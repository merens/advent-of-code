package aoc2016

import scala.util.matching.Regex

object Day03 {

  val triangleRegex: Regex = """\s*(\d+)\s*(\d+)\s*(\d+)""".r

  def isValidTriangle(a: Int, b: Int, c: Int): Boolean =
    a + b > c && a + c > b && b + c > a

  def countTriangles(lines: Seq[String]): Int =
    lines
      .map {
        case triangleRegex(a, b, c) => (a.toInt, b.toInt, c.toInt)
        case e                      => throw new IllegalArgumentException(s"error: $e")
      }
      .map { case (a, b, c) => isValidTriangle(a, b, c) }
      .count(_ == true)

  def parseVertical(lines: Seq[String]): Seq[Seq[Int]] =
    lines
      .map {
        case triangleRegex(a, b, c) => Seq(a.toInt, b.toInt, c.toInt)
        case e                      => throw new IllegalArgumentException(s"error: $e")
      }
      .transpose
      .flatMap(_.grouped(3).toSeq)

  def part2(lines: Seq[String]): Int =
    parseVertical(lines)
      .map { case Seq(a, b, c) => isValidTriangle(a, b, c) }
      .count(_ == true)

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day03.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countTriangles(parseInput(input)))
    println(part2(parseInput(input)))
  }
}
