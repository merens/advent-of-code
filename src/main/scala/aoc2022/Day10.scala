package aoc2022

import scala.util.matching.Regex

object Day10 {

  def main(args: Array[String]): Unit = {
    val initialRegisterX = 1
    println(part1(parseInput(input), initialRegisterX))
    part2(parseInput(input), initialRegisterX)
  }

  val addRegex: Regex = """addx (-?\d+)""".r

  def part1(instructions: Seq[String], register: Int): Int =
    executeInstructions(instructions, register)
      // zip starts with index 0, so add an element at the start of the cycles
      .prepended(0)
      .zipWithIndex
      .filter(_._2 % 40 == 20)
      .map { case (register, index) => register * index }
      .sum

  def executeInstructions(instructions: Seq[String], register: Int): Seq[Int] =
    instructions
      .scanLeft(Seq(register)) { (registerCycles, instruction) =>
        val register: Int = registerCycles.last
        instruction match {
          case "noop"      => Seq(register)
          case addRegex(n) => Seq(register, register + n.toInt)
        }
      }
      .flatten

  def part2(instructions: Seq[String], register: Int): Unit =
    executeInstructions(instructions, register)
      // I know there are 6 lines of 40 pixels
      .grouped(40)
      .map(_.zipWithIndex.map(pair => (pair._1, pair._2)))
      .take(6)
      .foreach { line =>
        println
        line.foreach {
//  https://www.reddit.com/r/adventofcode/comments/zhkiq9/2022_day_10_part_2_todays_puzzle_not_screenreader/izn29cd/
          case (register, crtPos) if (crtPos - 1 to crtPos + 1) contains register => print("⬜")
          case _                                                                  => print("⬛")
        }
      }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

}
