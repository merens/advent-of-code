package aoc2015

object Day20 {

  def parseInput(str: String): Int = str.toInt

  // brute force too slow
  // tried up to n/2
  // but optimized to sqrt(n) from https://rosettacode.org/wiki/Factors_of_an_integer#Scala
  def findDividers(n: Int): Seq[Int] = {
    val list: Seq[Int] = (1 to Math.sqrt(n).toInt)
      .filter(factor => n % factor == 0)
    list ++ list.reverse.dropWhile(factor => factor * factor == n).map(n / _)
  }

  def findDividersSum(n: Int, presentFactor: Int): Option[Int] =
    (1 to Integer.MAX_VALUE)
      .find(i => findDividers(i).sum * presentFactor >= n)

  def find10DividersSum(n: Int, presentFactor: Int): Option[Int] =
    (1 to Integer.MAX_VALUE)
      .find(i =>
        findDividers(i)
          .filter(factor => i / factor <= 50)
          .sum * presentFactor >= n
      )

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val n = parseInput(input)
    println(findDividersSum(n, 10))
    println(find10DividersSum(n, 11))
  }
}
