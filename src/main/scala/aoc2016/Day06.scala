package aoc2016

import scala.annotation.tailrec

object Day06 {

  def mostCommonChar(seq: Seq[Char]): Char =
    seq
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq
      // reverse sort + head
      .maxBy(_._2)
      ._1

  def leastCommonChar(seq: Seq[Char]): Char =
    seq
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq
      .minBy(_._2)
      ._1

  def part1(columns: Seq[Seq[Char]]): String =
    columns.map(mostCommonChar).mkString

  def part2(columns: Seq[Seq[Char]]): String =
    columns.map(leastCommonChar).mkString

  def parseInput(input: String): Seq[Seq[Char]] = input.linesIterator.toSeq.map(_.toSeq).transpose

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day06.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(part1(parseInput(input)))
    println(part2(parseInput(input)))
  }
}
