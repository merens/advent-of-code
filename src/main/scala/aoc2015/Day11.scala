package aoc2015

object Day11 {

  lazy val patterns: Seq[String] = ('a' to 'x').map(char => s"$char${(char + 1).toChar}${(char.toInt + 2).toChar}")
  def hasStraightOfLetters(s: String): Boolean = {
    patterns.exists(s.contains(_))
  }

  def hasConfusingLetters(s: String): Boolean =
    Seq('i', 'o', 'l').exists(confusingLetter => s.contains(confusingLetter))

  lazy val dups: Seq[String] = ('a' to 'z').map(letter => letter.toString * 2)
  def hasAtLeastTwoAdjacentDuplicate(s: String): Boolean =
    dups.count(s.contains(_)) >= 2

  def isValid(s: String): Boolean =
    !hasConfusingLetters(s) && hasStraightOfLetters(s) && hasAtLeastTwoAdjacentDuplicate(s)

  def increaseString(s: String): String =
    s.scanRight[(Boolean, Char)]((true, 'a')) { case (c: Char, (flag: Boolean, right: Char)) =>
      (flag, c) match {
        case (true, 'z')  => (true, 'a')
        case (true, char) => (false, (char.toInt + 1).toChar)
        case (_, char)   => (false, char)
      }
    }.dropRight(1)
      .map(_._2)
      .mkString

  def santaNextPassword(s: String): String = {
    var temp = increaseString(s)
    while (!isValid(temp)) {
      temp = increaseString(temp)
    }
    temp
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(santaNextPassword(input))
    println(santaNextPassword(santaNextPassword(input)))
  }
}
