package aoc2022

import scala.collection.mutable

object Day01 {

  def max(elves: Seq[Seq[Int]]): Int =
    elves.map(_.sum).max

  def top3(elves: Seq[Seq[Int]]): Int =
    elves.map(_.sum)
      .sorted
      .takeRight(3)
      .sum

  def parseInput(input: String): Seq[Seq[Int]] = {
    // the input pasted in txt had \r\n, in the test \n\n
    val regex = """(\r\n|\n|\r)\1""".r
    input.split(regex.toString())
      .toSeq
      .map(_.linesIterator.toSeq
        .map(_.toInt)
      )
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day01.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(max(parseInput(input)))
    println(top3(parseInput(input)))
  }
}
