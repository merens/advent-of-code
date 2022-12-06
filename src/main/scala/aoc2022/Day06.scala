package aoc2022

object Day06 {

  def part1(input: String): Int = {
    if (input.take(4).toSet.size == 4) 4
    else 1 + part1(input.drop(1))
  }

  def part2(input: String): Int = {
    if (input.take(14).toSet.size == 14) 14
    else 1 + part2(input.drop(1))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day06.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(part1(input))
    println(part2(input))
  }
}
