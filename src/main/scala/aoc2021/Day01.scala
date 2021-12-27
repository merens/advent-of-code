package aoc2021

import utils.IteratorImplicits._

object Day01 {

  def countIncreases(depths: Seq[Int], window: Int = 1): Int = {
    depths.sliding(window).map(_.sum).zipWithTail.count({ case (a, b) => a < b })
  }

  def parseDepths(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day01.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countIncreases(parseDepths(input)))
    println(countIncreases(parseDepths(input), 3))
  }
}