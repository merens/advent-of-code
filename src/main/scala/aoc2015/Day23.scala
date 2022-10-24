package aoc2015

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day23 {

  // I don't like the way this state is initialized
  case class State(registers: Map[String, Int] = Map("a" -> 0, "b" -> 0), index: Int = 0)

  val halfRegex: Regex      = """hlf (\w)""".r
  val tripleRegex: Regex    = """tpl (\w)""".r
  val incrementRegex: Regex = """inc (\w)""".r
  val jumpRegex: Regex      = """jmp ([+-]\d+)""".r
  val jieRegex: Regex       = """jie (\w), ([+-]\d+)""".r
  val jioRegex: Regex       = """jio (\w), ([+-]\d+)""".r

  def logic(state: State, instruction: String): State = instruction match {
    // TODO: consider using a map instead of two attributes in State, this way I can access directly after parsing the instruction
    // now I can generalize with more registers, but I still have duplicated code because I check the register status in 5 cases/6
    // this match case feels big, should refactor and call different functions for each instruction type
    case halfRegex(register)        =>
      state.copy(
        registers = state.registers + (register -> state.registers(register) / 2),
        index = state.index + 1
      )
    case tripleRegex(register)      =>
      state.copy(
        registers = state.registers + (register -> state.registers(register) * 3),
        index = state.index + 1
      )
    case incrementRegex(register)   =>
      state.copy(
        registers = state.registers + (register -> (state.registers(register) + 1)),
        index = state.index + 1
      )
    case jumpRegex(offset)          => state.copy(index = state.index + Integer.parseInt(offset))
    case jieRegex(register, offset) =>
      if (state.registers(register) % 2 == 0)
        state.copy(index = state.index + Integer.parseInt(offset))
      else state.copy(index = state.index + 1)
    case jioRegex(register, offset) =>
      if (state.registers(register) == 1)
        state.copy(index = state.index + Integer.parseInt(offset))
      else state.copy(index = state.index + 1)

    case e => throw new IllegalArgumentException(s"parsing error: $e")
  }

  @tailrec
  def core(state: State, instructions: Seq[String]): Int =
    if (state.index < 0 || state.index >= instructions.size) state.registers("b")
    else {
      val newState = logic(state, instructions(state.index))
      core(newState, instructions)
    }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val instructions               = input.linesIterator.toSeq
    val initialState: State        = State()
    println(core(initialState, instructions))
    val initialStateForPar2: State = State(registers = Map("a" -> 1, "b" -> 0))
    println(core(initialStateForPar2, instructions))
  }
}
