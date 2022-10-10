package aoc2015

import scala.collection.mutable

object Day01 {

  def countParenthesis(directions: String): Int =
    directions.count(_.equals('(')) - directions.count(_.equals(')'))

  // with this we return the position of the first unbalanced ')'
  def findBasement(directions: String): Int = {
    val q: mutable.Queue[Char] = mutable.Queue.empty
    var result                 = 1
    directions.toVector.foreach {
      case e @ '(' =>
        q.enqueue(e)
        result = result + 1
      case _       =>
        if (q.isEmpty) return result
        else {
          q.dequeue()
          result = result + 1
        }
    }
    -1
  }

  // alternative: with scanLeft we map at each parenthesis the floor that we are at
  def alternative(directions: String): Int =
    directions
      .scanLeft(0) { (floor, direction) =>
        direction match {
          case '(' => floor + 1
          case ')' => floor - 1
        }
      }
      .indexOf(-1)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day01.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countParenthesis(input))
    println(findBasement(input))
    println(alternative(input))
  }
}
