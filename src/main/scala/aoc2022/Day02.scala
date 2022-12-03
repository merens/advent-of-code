package aoc2022

object Day02 {

  // Rock, Paper, Scissors

  // I don't like these maps or the alternative with obscure operations between Chars
  val winMap: Map[Char, Char]  = Map('A' -> 'Y', 'B' -> 'Z', 'C' -> 'X')
  val loseMap: Map[Char, Char] = Map('A' -> 'Z', 'B' -> 'X', 'C' -> 'Y')

  def calcScore(result: Char, myMove: Char): Int = {
    result match {
      case 2 => 6 + (myMove - ('X' - 1))
      case 1 => 3 + (myMove - ('X' - 1))
      case 0 => 0 + (myMove - ('X' - 1))
    }
  }

  def rPSscore(opponentMove: Char, myMove: Char): Int = {
    val offset = 'X' - 'A'
    val diff   = myMove - opponentMove
    if (diff == offset) calcScore(1, myMove)
    else if (diff == offset + 1 || diff == offset - 2) calcScore(2, myMove)
    else calcScore(0, myMove)
  }

  def rPSscore2(opponentMove: Char, myMove: Char): Int = {
    val offset = 'X' - 'A'
    val diff   = myMove - opponentMove
    val result = {
      if (diff == offset) 3
      else if ((diff - offset) % 3 == 1) 6
      else 0
    }
    (myMove - ('X' - 1)) + result
  }

  def chooseOutcome(opponentMove: Char, outcome: Char): Int = {
    val offset = 'X' - 'A'
    outcome match {
      case 'X' => calcScore(0, loseMap(opponentMove))
      case 'Y' => calcScore(1, (opponentMove + offset).toChar)
      case 'Z' => calcScore(2, winMap(opponentMove))
    }

  }

  def map(instructions: Seq[Seq[Char]], function: (Char, Char) => Int): Int =
    instructions.map { case Seq(o, m) =>
      function(o, m)
    }.sum

  // I don't like this parsing of Chars
  def parseInput(input: String): Seq[Seq[Char]] = input.linesIterator.map(_.split(" ").toSeq.map(_.head)).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day02.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(map(parseInput(input), rPSscore))
    println(map(parseInput(input), chooseOutcome))
  }
}
