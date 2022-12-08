package aoc2016

import utils.{ Grid, Pos }
import utils.GridImplicits._

object Day02 {

  val grid: Grid[Int] = (1 to 9).toVector.grouped(3).toVector

  val gridPart2: Grid[String] = Vector(
    Vector("0", "0", "1", "0", "0"),
    Vector("0", "2", "3", "4", "0"),
    Vector("5", "6", "7", "8", "9"),
    Vector("0", "A", "B", "C", "0"),
    Vector("0", "0", "D", "0", "0")
  )
  val allowedValues: Seq[Pos] = Seq(
    Pos(2, 0),
    Pos(1, 1), Pos(2, 1), Pos(3, 1),
    Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(3, 2), Pos(4, 2),
    Pos(1, 3), Pos(2, 3), Pos(3, 3),
    Pos(2, 4)
  )

  def nextPos(position: Pos, instruction: Char): Pos =
    instruction match {
      case 'U' =>
        Pos(Math.max(0, position.x - 1), position.y)
      case 'D' =>
        Pos(Math.min(2, position.x + 1), position.y)
      case 'L' =>
        Pos(position.x, Math.max(0, position.y - 1))
      case 'R' =>
        Pos(position.x, Math.min(2, position.y + 1))
      case e   => throw new IllegalArgumentException(s"error: $e")
    }

  def nextPosPart2(position: Pos, instruction: Char): Pos = {
    val newPos = instruction match {
      case 'U' => Pos(position.x - 1, position.y)
      case 'D' => Pos(position.x + 1, position.y)
      case 'L' => Pos(position.x, position.y - 1)
      case 'R' => Pos(position.x, position.y + 1)
      case e => throw new IllegalArgumentException(s"error: $e")
    }
    if (allowedValues.contains(newPos)) newPos else position
  }

  def linesFunction[A](instructions: Seq[String], f: (Pos, Char) => Pos, keypad: Grid[A]): Seq[A] =
    instructions
      .scanLeft(Pos(1, 1)) { (position, instruction) =>
        instruction.foldLeft(position)((position, char) =>
          f(position, char))
      }
      .map(keypad(_))
      .tail

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day02.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(linesFunction(parseInput(input), nextPos, grid).mkString)
    println(linesFunction(parseInput(input), nextPosPart2, gridPart2).mkString)
  }
}
