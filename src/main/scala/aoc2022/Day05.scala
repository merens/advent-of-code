package aoc2022

import scala.util.matching.Regex

object Day05 {

  //  example: move 3 from 7 to 4
  val instructionRegex: Regex = """move (\d+) from (\d) to (\d)""".r

  def encodeArrangement(arrangement: Seq[Seq[String]]): String =
    arrangement.map(_.head).mkString

  def execute(initialArrangement: Seq[Seq[String]], instructions: Seq[String], isReversed: Boolean): Seq[Seq[String]] =
    instructions
      .foldLeft(initialArrangement)((config, instruction) => moveCargo(config, instruction, isReversed))

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
      case ""                        => config
      case e                         => throw new IllegalArgumentException(s"error parsing instructions: $e")
    }

  def part1(initialArrangement: Seq[Seq[String]], instructions: Seq[String]): String =
    encodeArrangement(execute(initialArrangement, instructions, isReversed = true))

  def part2(initialArrangement: Seq[Seq[String]], instructions: Seq[String]): String =
    encodeArrangement(execute(initialArrangement, instructions, isReversed = false))

  def parseCargo(input: Seq[String]): Seq[Seq[String]] =
    input.transpose
      // input has fixed size and every 4 spaces there is a cargo stack
      .zipWithIndex
      .filter(_._2 % 4 == 1)
      .map(_._1)
      // filter out numbers and empty spaces
      .map(
        _.filter(_.isUpper)
        // it's faster on Seq(String) than on Seq(Char)
          .map(_.toString)
      )

  lazy val input: Seq[String] =
    io.Source.fromInputStream(getClass.getResourceAsStream("day05.txt")).mkString.trim.linesIterator.toSeq

  def main(args: Array[String]): Unit = {
    val (cargoInput, instructionsInput): (Seq[String], Seq[String]) = input.splitAt(input.indexOf(""))
    println(part1(parseCargo(cargoInput), instructionsInput))
    println(part2(parseCargo(cargoInput), instructionsInput))
  }
}
