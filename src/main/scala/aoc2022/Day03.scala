package aoc2022

object Day03 {

  def part1(compartments: Seq[String]): Int =
    compartments
      .map(findDuplicateCompartments)
      .map(calcPriority)
      .sum

  def part2(compartments: Seq[String]): Int =
    compartments
      .grouped(3)
      .toSeq
      .map(findDuplicateGroup)
      .map(calcPriority)
      .sum

  def findDuplicateCompartments(compartmentPair: String): Char = {
    val tuple = compartmentPair.splitAt(compartmentPair.length / 2)
    tuple._1 intersect tuple._2
  }.head

  def findDuplicateGroup(group: Seq[String]): Char = {
    group.head intersect group(1) intersect group.last
  }.head

  def calcPriority(c: Char): Int = c match {
    case c: Char if c.isLower => c - 'a' + 1
    case c: Char if c.isUpper => c - 'A' + 1 + 26
    case _                    => throw new Exception("a")
  }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day03.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(part1(parseInput(input)))
    println(part2(parseInput(input)))
  }
}
