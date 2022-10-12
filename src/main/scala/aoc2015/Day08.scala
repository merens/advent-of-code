package aoc2015

object Day08 {

  // TODO: readability? alternatives to regex?
  def countSlashChars(s: String): Int =
    "\\\\\\\\".r.findAllIn(s).toSeq.size + "\\\\\"".r.findAllIn(s).toSeq.size

  def countHexChars(s: String): Int =
    "\\\\x[\\da-f]{2}".r.findAllIn(s).toSeq.size

  def countTotalEscapeds(strings: Seq[String]): Int =
    strings.map { s =>
      val stringContent = s.drop(1).dropRight(1)
      2 + countSlashChars(stringContent) + countHexChars(stringContent) * 3
    }.sum

  def countTotalEscapedChars(strings: Seq[String]): Int =
    strings.map { s =>
      val stringContent = s.drop(1).dropRight(1)
      4 + countSlashChars(stringContent) * 2 + countHexChars(stringContent)
    }.sum

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day08.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countTotalEscapeds(parseInput(input)))
    println(countTotalEscapedChars(parseInput(input)))
  }
}
