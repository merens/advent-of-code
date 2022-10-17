package aoc2015

import scala.::

object Day10 {

  def look(s: String, seq: Seq[Char]): Seq[Int] = {
    val temp = seq
      .scanLeft(0) { (partial: Int, n: Char) =>
        s.indexWhere(_ != n, partial)
      }
  // TODO: simplify this logic
      .tail
      .dropRight(1) :+ s.length
    temp.head +: temp.zip(temp.tail).map { case (a, b) => b - a }
  }

  def say(occurrences: Seq[Int], numbers: Seq[Char]): String =
    occurrences.zip(numbers).map { case (a, b) => s"$a$b" }.mkString

  def lookAndSay(s: String): String = {
    val distinct = distinctNumbers(s)
    say(look(s, distinct), distinct)
  }


  def distinctNumbers(s: String): Seq[Char] = {
    val seq = s.toSeq
    seq.head +: seq.sliding(2).collect { case Seq(a, b) if a != b => b }.toSeq
  }

  // interesting way to iterate a function, passing down the result
  def repeated[A](f: A => A, n: Int): A => A = { (a0: A) =>
    var res: A = a0
    for (i <- 0 until n) {
      res = f(res)
//      println(res)
    }
    res
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(repeated(lookAndSay, 40)(input).length)
    println(repeated(lookAndSay, 50)(input).length)
  }
}
