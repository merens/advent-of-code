package aoc2022

import scala.util.matching.Regex

object Day05 {

  //  val cargoRegex: Regex = """\[(\w)]""".r
  //  example: move 3 from 7 to 4
  val instructionRegex: Regex   = """move (\d+) from (\d) to (\d)""".r
  val doubleNewLineRegex: Regex = """(\r\n|\n|\r)\1""".r

  // TODO: parse from file instead of using hardcoded input
  val cargos: Seq[Seq[String]] = Seq(
    Seq("T", "F", "V", "Z", "C", "W", "S", "Q"),
    Seq("B", "R", "Q"),
    Seq("S", "M", "P", "Q", "T", "Z", "B"),
    Seq("H", "Q", "R", "F", "V", "D"),
    Seq("P", "T", "S", "B", "D", "L", "G", "J"),
    Seq("Z", "T", "R", "W"),
    Seq("J", "R", "F", "S", "N", "M", "Q", "H"),
    Seq("W", "H", "F", "N", "R"),
    Seq("B", "R", "P", "Q", "T", "Z", "J")
  )

  def encodeArrangement(arrangement: Seq[Seq[String]]): String =
    arrangement.map(_.head).mkString

  def execute(initialArrangement: Seq[Seq[String]], instructions: Seq[String], isReversed: Boolean): Seq[Seq[String]] =
    instructions
      .foldLeft(initialArrangement)((config, instruction) => moveCargo(config, instruction, isReversed))

  def execAndEncode(initialArrangement: Seq[Seq[String]], instructions: Seq[String], isReversed: Boolean): String =
    encodeArrangement(execute(initialArrangement, instructions, isReversed))

  def moveCargo(config: Seq[Seq[String]], instruction: String, isReversed: Boolean): Seq[Seq[String]] =
    instruction match {
      case instructionRegex(a, b, c) =>
        val quantity         = a.toInt
        val firstStackIndex  = b.toInt - 1
        val secondStackIndex = c.toInt - 1
        val stackToBeMoved   =
          if (isReversed) config(firstStackIndex).take(quantity).reverse
          else config(firstStackIndex).take(quantity)
        val newSecondStack   = config(secondStackIndex).prependedAll(stackToBeMoved)
        val newFirstStack    = config(firstStackIndex).drop(quantity)
        config
          .updated(firstStackIndex, newFirstStack)
          .updated(secondStackIndex, newSecondStack)
      case e                         => throw new IllegalArgumentException(s"error parsing instructions: $e")
    }

  //  def parseArrangementuration(input: String) = input.linesIterator.toSeq

  def parseInstructions(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day05.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val instructions = parseInstructions(input.split(doubleNewLineRegex.toString).toSeq.last)
    println(execAndEncode(cargos, instructions, isReversed = true))
    println(execAndEncode(cargos, instructions, isReversed = false))
  }
}
