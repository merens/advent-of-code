package aoc2022

object Day06 {

  // assumes that a sequence of non-repeating chars always exists in the string
  def findNonRepeatingSequence(input: String, n: Int): Int =
    if (input.take(n).toSet.size == n) n
    else findNonRepeatingSequence(input.drop(1), n) + 1

  def part1(input: String): Int = {
    findNonRepeatingSequence(input, 4)
  }

  def part2(input: String): Int = {
    findNonRepeatingSequence(input, 14)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day06.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(part1(input))
    println(part2(input))
  }
}
