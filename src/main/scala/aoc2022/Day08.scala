package aoc2022

import utils.GridImplicits._
import utils.{ Grid, Pos }

object Day08 {

  def main(args: Array[String]): Unit = {
    println(part1(parseInput(input)))
    println(part2(parseInput(input)))
  }

  def part1(grid: Grid[Int]): Int = {
    val transpose: Grid[Int] = grid.transpose
    grid.indices
      .flatMap(x =>
        grid.head.indices
          .map(Pos(x, _))
      )
      .map { pos: Pos =>
        isVisible(grid(pos.x).take(pos.y + 1), grid(pos)) ||
        isVisible(grid(pos.x).drop(pos.y), grid(pos)) ||
        isVisible(transpose(pos.y).take(pos.x + 1), grid(pos)) ||
        isVisible(transpose(pos.y).drop(pos.x), grid(pos))
      }
      .count(_ == true)
  }

  def isVisible(sequence: Seq[Int], n: Int): Boolean =
    sequence.count(_ == n) == 1 && sequence.max == n

  def part2(grid: Grid[Int]): Int = {
    val transpose: Grid[Int] = grid.transpose
    grid.indices
      .flatMap(x =>
        grid.head.indices
          .map(Pos(x, _))
      )
      .map { pos: Pos =>
        viewingDistance(grid(pos.x).take(pos.y).reverse, grid(pos)) *
          viewingDistance(grid(pos.x).drop(pos.y + 1), grid(pos)) *
          viewingDistance(transpose(pos.y).take(pos.x).reverse, grid(pos)) *
          viewingDistance(transpose(pos.y).drop(pos.x + 1), grid(pos))
      }
      .max
  }

  def viewingDistance(sequence: Seq[Int], n: Int): Int =
    Math.min(
      sequence.takeWhile(_ < n).size + 1,
      sequence.size
    )

  def parseInput(input: String): Grid[Int] =
    input.linesIterator
      .map(_.toVector.map(_.toString.toInt))
      .toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day08.txt")).mkString.trim

}
