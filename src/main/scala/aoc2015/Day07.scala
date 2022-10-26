package aoc2015

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object Day07 {

  // https://www.reddit.com/r/adventofcode/comments/3vr4m4/day_7_solutions/cxqifa2/
  sealed trait Expression
  case class Const(value: Int)  extends Expression
  case class Wire(name: String) extends Expression

  case class Not(expression: Expression)              extends Expression
  case class And(left: Expression, right: Expression) extends Expression
  case class Or(left: Expression, right: Expression)  extends Expression
  case class LShift(expression: Expression, by: Int)  extends Expression
  case class RShift(expression: Expression, by: Int)  extends Expression

  val wireRegex: Regex = """(\w+) -> (\w+)""".r
  val andRegex: Regex = """(\w+) AND (\w+) -> (\w+)""".r
  val orRegex: Regex = """(\w+) OR (\w+) -> (\w+)""".r
  val lShiftRegex: Regex = """(\w+) LSHIFT (\d+) -> (\w+)""".r
  val rShiftRegex: Regex = """(\w+) RSHIFT (\d+) -> (\w+)""".r
  val notRegex: Regex = """NOT (\w+) -> (\w+)""".r

  def constOrVariable(rawExpr: String): Expression = Try(rawExpr.toInt) match {
    case Success(value) => Const(value)
    case Failure(_)     => Wire(rawExpr)
  }

  def parseInstructions(rawInstructions: Seq[String]): Map[String, Expression] = rawInstructions.map {
    case wireRegex(from, to)       => to -> constOrVariable(from)
    case andRegex(left, right, to) => to -> And(constOrVariable(left), constOrVariable(right))
    case orRegex(left, right, to)  => to -> Or(constOrVariable(left), constOrVariable(right))
    case lShiftRegex(from, by, to) => to -> LShift(constOrVariable(from), by.toInt)
    case rShiftRegex(from, by, to) => to -> RShift(constOrVariable(from), by.toInt)
    case notRegex(from, to)        => to -> Not(constOrVariable(from))
  }.toMap

  def evaluate(wires: Map[String, Expression], expr: Expression): Int = {
    val mutWires = scala.collection.mutable.Map() ++ wires

    def eval(expr: Expression): Int = expr match {
      case Const(value)  => value
      case Wire(name)    =>
        val value = eval(mutWires(name))
        mutWires(name) = Const(value)
        value
      case Not(e)        => ~eval(e)
      case And(l, r)     => eval(l) & eval(r)
      case Or(l, r)      => eval(l) | eval(r)
      case LShift(e, by) => eval(e) << by
      case RShift(e, by) => eval(e) >> by
    }

    eval(expr)
  }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day07.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val a = evaluate(parseInstructions(parseInput(input)), Wire("a"))
    println(a)

    val newInstructions = parseInstructions(parseInput(input)).updated("b", Const(a))
    val newA            = evaluate(newInstructions, Wire("a"))
    println(newA)
  }
}
