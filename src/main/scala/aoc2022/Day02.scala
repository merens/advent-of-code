package aoc2022

object Day02 {

  // Rock, Paper, Scissors
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

  def chooseOutcome(opponentMove: Char, outcome: Char): Int = {
    val offset = 'X' - 'A'
    outcome match {
      case 'Z' => calcScore(2, winMap(opponentMove))
      case 'Y' => calcScore(1, (opponentMove + offset).toChar)
      case 'X' => calcScore(0, loseMap(opponentMove))
    }

  }

  def map(instructions: Seq[Seq[Char]], function: (Char, Char) => Int): Int =
    instructions.map { case Seq(o, m) =>
      function(o, m)
    }.sum

  def parseInput(input: String): Seq[Seq[Char]] = input.linesIterator.map(_.split(" ").toSeq.map(_.head)).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day02.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(map(parseInput(input), rPSscore))
    println(map(parseInput(input), chooseOutcome))
  }
}
