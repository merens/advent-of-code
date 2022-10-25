package aoc2015

object Day25 {

  //  * 252533
  //  % 33554393
  // 2 parts:
  //  - find index in the matrix from row and column
  //  - iterate * and %
  //  dynamic: if the index is a lot bigger than 33554393, it's better to save all the possible results in a LUT https://en.wikipedia.org/wiki/Lookup_table
  //    it's too big :(
  def createLUT(n: Int): Map[Int, Int]    =
    (0 until n).foldLeft(Map.empty[Int, Int])((map, i) => map + (i -> (i.toLong * 252533 % 33554393).toInt))

  def toIndex(row: Int, column: Int): Int =
    (1 to column - 1 + row - 1).sum + column

  def parseInput(input: String): Seq[Int] =
    input.linesIterator.toSeq.map(Integer.parseInt)

  def f(n: Int, lut: Map[Int, Int]): Int = lut(n)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val lut      = createLUT(33554393)
    val starting = 20151125
    //TODO: parse input
    // slow: 2 mins
    println(Day10.repeated(f(_, lut), toIndex(2978, 3083) - 1)(starting))
  }
}
