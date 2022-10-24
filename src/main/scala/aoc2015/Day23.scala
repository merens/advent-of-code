package aoc2015

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day23 {

  // computer
  // 2 registers for non-negative ints: a, b
  //    start at 0
  // 6 instructions:
  //  hlf r: half register r
  //  tpl r: triple register r
  //  inc r: increment register r
  //  jmp offset: jump to instruction offset away
  //  jie r, offset: jump offset if value in register r is even
  //  jio r, offset: jump offset if value in register r is 1
  // offset has sign +/- that indicates direction of jump
  //  jmp +1: simply next instruction
  //  jmp +0: infinite loop
  // exit when outside of defined instructions
  // print value in b

  // parse all instructions. N.B.: 4 are unary, 2 are binary
  // keep 3 vars: a, b, index(of next instruction to be executed)
  // implement instructions logic, unit tests?
  // loop
  // part 2? ez
  case class State(a: Int = 0, b: Int = 0, index: Int = 0)

  val jumpRegex: Regex = """jmp ([+-]\d+)""".r
  val jieRegex: Regex  = """jie (\w), ([+-]\d+)""".r
  val jioRegex: Regex  = """jio (\w), ([+-]\d+)""".r

  def logic(state: State, instruction: String): State = instruction match {
    // TODO: consider using a map instead of two attributes in State, this way I can access directly after parsing the instruction
    case """hlf a"""                                   => state.copy(a = state.a / 2, index = state.index + 1)
    case """hlf b"""                                   => state.copy(b = state.b / 2, index = state.index + 1)
    case """tpl a"""                                   => state.copy(a = state.a * 3, index = state.index + 1)
    case """tpl b"""                                   => state.copy(b = state.b * 3, index = state.index + 1)
    case """inc a"""                                   => state.copy(a = state.a + 1, index = state.index + 1)
    case """inc b"""                                   => state.copy(b = state.b + 1, index = state.index + 1)
    case jumpRegex(offset)                             => state.copy(index = state.index + Integer.parseInt(offset))
    case jieRegex(register, offset) if register == "a" =>
      if (state.a % 2 == 0) state.copy(index = state.index + Integer.parseInt(offset))
      else state.copy(index = state.index + 1)
    case jieRegex(register, offset) if register == "b" =>
      if (state.b % 2 == 0) state.copy(index = state.index + Integer.parseInt(offset))
      else state.copy(index = state.index + 1)
    case jioRegex(register, offset) if register == "a" =>
      if (state.a == 1) state.copy(index = state.index + Integer.parseInt(offset))
      else state.copy(index = state.index + 1)
    case jioRegex(register, offset) if register == "b" =>
      if (state.b == 1) state.copy(index = state.index + Integer.parseInt(offset))
      else state.copy(index = state.index + 1)

    case e => throw new IllegalArgumentException(s"parsing error: $e")
  }

  @tailrec
  def core(state: State, instructions: Seq[String]): Int =
    if (state.index < 0 || state.index >= instructions.size) state.b
    else {
      val newState = logic(state, instructions(state.index))
      core(newState, instructions)
    }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val instructions               = input.linesIterator.toSeq
    val initialState: State        = State()
    println(core(initialState, instructions))
    val initialStateForPar2: State = State(a = 1)
    println(core(initialStateForPar2, instructions))
  }
}
