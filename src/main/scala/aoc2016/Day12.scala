package aoc2016

import aoc2015.Day23.State

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day12 {

  // assembunny = assembly
  //  similar to 2015d23
  // 4 registers: a, b, c, d
  // instructions:
  //  cpy x y: x could be an integer or the value of a register
  //  inc x
  //  dec x
  //  jnz x y: jump to an instruction y away, x sign gives the direction, x must not be 0

  // I'm using State from 2015d23, instructions are different and we have more registers
  //  and copied
  // TODO: consider assembly astraction

  val cpyIntRegex: Regex = """cpy (\d+) (\w)""".r
  val cpyRegisterRegex: Regex = """cpy (\w) (\w)""".r
  val incrementRegex: Regex = """inc (\w)""".r
  val decrementRegex: Regex = """dec (\w)""".r
  val jnzRegex: Regex = """jnz (\d+) (-?\d+)""".r
  val jnzRegisterRegex: Regex = """jnz (\w) (-?\d+)""".r

  def logic(state: State, instruction: String): State = instruction match {
    case cpyIntRegex(n, register) =>
      state.copy(
        registers = state.registers + (register -> n.toInt),
        index = state.index + 1
      )
    case cpyRegisterRegex(from, to) =>
      state.copy(
        registers = state.registers + (to -> state.registers(from)),
        index = state.index + 1
      )
    case incrementRegex(register) =>
      state.copy(
        registers = state.registers + (register -> (state.registers(register) + 1)),
        index = state.index + 1
      )
    case decrementRegex(register) =>
      state.copy(
        registers = state.registers + (register -> (state.registers(register) - 1)),
        index = state.index + 1
      )
    case jnzRegex(n, offset) =>
      if (n.toInt != 0)
        state.copy(index = state.index + Integer.parseInt(offset) * n.toInt.sign)
      else state.copy(index = state.index + 1)
    case jnzRegisterRegex(register, offset) =>
      val valueRegister = state.registers(register)
      if (valueRegister != 0)
        state.copy(index = state.index + Integer.parseInt(offset) * valueRegister.sign)
      else state.copy(index = state.index + 1)
    case e => throw new IllegalArgumentException(s"error parsing instruction: $e")
  }

  @tailrec
  def core(state: State, instructions: Seq[String]): Int =
    if (state.index < 0 || state.index >= instructions.size) state.registers("a")
    else {
      val newState = logic(state, instructions(state.index))
      core(newState, instructions)
    }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val instructions = input.linesIterator.toSeq
    val initialState: State = State(registers = Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0))
    println(core(initialState, instructions))
    val initialStateForPar2: State = State(registers = Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0))
    println(core(initialStateForPar2, instructions))
  }
}
