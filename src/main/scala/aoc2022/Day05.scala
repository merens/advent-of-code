package aoc2022

import scala.util.matching.Regex

object Day05 {

  //  example: move 3 from 7 to 4
  val instructionRegex: Regex = """move (\d+) from (\d) to (\d)""".r

  // TODO: parse from file instead of using hardcoded input
  val cargos: Map[Int, Seq[String]] = Map(
    1 -> Seq("T", "F", "V", "Z", "C", "W", "S", "Q"),
    2 -> Seq("B", "R", "Q"),
    3 -> Seq("S", "M", "P", "Q", "T", "Z", "B"),
    4 -> Seq("H", "Q", "R", "F", "V", "D"),
    5 -> Seq("P", "T", "S", "B", "D", "L", "G", "J"),
    6 -> Seq("Z", "T", "R", "W"),
    7 -> Seq("J", "R", "F", "S", "N", "M", "Q", "H"),
    8 -> Seq("W", "H", "F", "N", "R"),
    9 -> Seq("B", "R", "P", "Q", "T", "Z", "J")
  )

  case class Config(config: Map[Int, Seq[String]])

  def part1(initialConfig: Config, instructions: Seq[String]): String = {
    val result =
      instructions.foldLeft(initialConfig)((config, instruction) => moveCargoReversed(config, instruction)).config
    (1 to 9).map(result(_)).map(_.head).mkString
  }

  def part2(initialConfig: Config, instructions: Seq[String]): String = {
    val result =
      instructions.foldLeft(initialConfig)((config, instruction) => moveCargo(config, instruction)).config
    (1 to 9).map(result(_)).map(_.head).mkString
  }

  def moveCargoReversed(config: Config, instruction: String): Config =
    instruction match {
      case instructionRegex(a, b, c) =>
        val toBeMoved = config.config(b.toInt).take(a.toInt).reverse
        val newC      = config.config(c.toInt).prependedAll(toBeMoved)
        val newB      = config.config(b.toInt).drop(a.toInt)
        Config(config.config + (b.toInt -> newB) + (c.toInt -> newC))
    }

  def moveCargo(config: Config, instruction: String): Config =
    instruction match {
      case instructionRegex(a, b, c) =>
        val toBeMoved = config.config(b.toInt).take(a.toInt)
        val newC      = config.config(c.toInt).prependedAll(toBeMoved)
        val newB      = config.config(b.toInt).drop(a.toInt)
        Config(config.config + (b.toInt -> newB) + (c.toInt -> newC))
    }


  def parseInstructions(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day05.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val regex        = """(\r\n|\n|\r)\1""".r
    val instructions = parseInstructions(input.split(regex.toString()).toSeq.last)
    println(part1(Config(cargos), instructions))
    println(part2(Config(cargos), instructions))
  }
}
