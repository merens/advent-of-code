package aoc2016

import aoc2016.Day01.Directions.{ Direction, E, N, S, W }

import scala.util.matching.Regex

object Day01 {

  case class Instruction(direction: String, blocks: Int)

  object Directions extends Enumeration {
    type Direction = Value
    val N, S, W, E = Value
  }
  case class Status(x: Int, y: Int, direction: Direction)

  val instructionRegex: Regex = """([LR])(\d+)""".r

  def parseInput(input: String): Seq[Instruction] = input
    .split(",")
    .map(_.trim)
    .map { case instructionRegex(direction, n) =>
      Instruction(direction, n.toInt)
    }

  def walk(seq: Seq[Instruction], initialStatus: Status): Status =
    seq.foldLeft(initialStatus)((status, instruction) => nextStatus(status, instruction))

  def nextStatus(status: Status, instruction: Instruction): Status =
    (status.direction, instruction.direction) match {
      case (N, "L") => status.copy(x = status.x - instruction.blocks, direction = W)
      case (N, "R") => status.copy(x = status.x + instruction.blocks, direction = E)
      case (S, "L") => status.copy(x = status.x + instruction.blocks, direction = E)
      case (S, "R") => status.copy(x = status.x - instruction.blocks, direction = W)
      case (W, "L") => status.copy(y = status.y - instruction.blocks, direction = S)
      case (W, "R") => status.copy(y = status.y + instruction.blocks, direction = N)
      case (E, "L") => status.copy(y = status.y + instruction.blocks, direction = N)
      case (E, "R") => status.copy(y = status.y - instruction.blocks, direction = S)
      case e        => throw new IllegalArgumentException(s"error: $e")
    }

  def path(seq: Seq[Instruction], initialStatus: Status): Seq[Status] =
    seq.scanLeft(initialStatus)((status, instruction) => nextStatus(status, instruction))

  def allPath(sequence: Seq[Status]): Seq[(Int, Int)] =
    sequence.zip(sequence.tail).flatMap {
      case (a, b) if a.x == b.x => (a.y until b.y by ((b.y - a.y) / Math.abs(b.y - a.y))).map((a.x, _))
      case (a, b) if a.y == b.y => (a.x until b.x by ((b.x - a.x) / Math.abs(b.x - a.x))).map((_, a.y))
    }

  def findFirstDuplicate(seq: Seq[(Int, Int)]): Option[(Int, Int)] =
    (0 to seq.size).find(i => seq.indexOf(seq(i), i + 1) >= 0).map(seq(_))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day01.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val initialStatus: Status = Status(0, 0, N)
    val result                = walk(parseInput(input), initialStatus)
    println(Math.abs(result.x) + Math.abs(result.y))
    val pathPoints            = path(parseInput(input), initialStatus)
    val pathSeq               = allPath(pathPoints)
    val (x, y)                = findFirstDuplicate(pathSeq).getOrElse(0, 0)
    println(Math.abs(x) + Math.abs(y))
  }

}
