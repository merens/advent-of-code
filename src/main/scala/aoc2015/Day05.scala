package aoc2015

object Day05 {

  def atLeast3Vowels(s: String): Boolean =
    Seq('a', 'e', 'i', 'o', 'u').map(vowel => s.count(_ == vowel)).sum >= 3

  def hasDouble(s: String): Boolean = ('a' to 'z').map(letter => letter.toString * 2).exists(s.contains(_))

  def doesNotContainNaughtyPairs(s: String): Boolean = !Seq("ab", "cd", "pq", "xy").exists(s.contains(_))

  def isNice(s: String): Boolean =
    atLeast3Vowels(s) && hasDouble(s) && doesNotContainNaughtyPairs(s)

  def countNice(words: Seq[String]): Int =
    words.map(isNice).count(_ == true)

  // Part 2
  // Note that findAllIn finds matches that don't overlap, you want findAllMatchIn for non-overlapping
  def hasDoublePair(s: String): Boolean = s.sliding(2).toSeq.exists(pattern => pattern.r.findAllIn(s).size >= 2)

  def hasXyX(s: String): Boolean = ('a' to 'z').exists(letter => s"$letter.$letter".r.findFirstIn(s).isDefined)

  def isNice2(s: String): Boolean =
    hasDoublePair(s) && hasXyX(s)

  def countNice2(words: Seq[String]): Int =
    words.map(isNice2).count(_ == true)


  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day05.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countNice(parseInput(input)))
    println(countNice2(parseInput(input)))
  }
}
