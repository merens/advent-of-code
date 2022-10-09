package aoc2015

object Day02 {

  def misurePaper(dimensionList: Seq[Seq[Int]]): Seq[Int] =
    dimensionList
      .scanLeft(0) { (paper, dimensions) =>
        dimensions match {
          case Seq(l: Int, w: Int, h: Int) => paper + surfaceArea(l, w, h) + smallestSide(l, w, h)
          case _                           => throw new IllegalArgumentException("illegal dimension")
        }
      }

  def misureRibbon(dimensionList: Seq[Seq[Int]]): Seq[Int] =
    dimensionList
      .scanLeft(0) { (paper, dimensions) =>
        dimensions match {
          case Seq(l: Int, w: Int, h: Int) => paper + volume(l, w, h) + smallestPerimeter(l, w, h)
          case _                           => throw new IllegalArgumentException("illegal dimension")
        }
      }

  def surfaceArea(length: Int, width: Int, height: Int): Int =
    2 * (length * width + width * height + height * length)

  def smallestSide(length: Int, width: Int, height: Int): Int =
    Seq(length * width, width * height, height * length).min

  def smallestPerimeter(length: Int, width: Int, height: Int): Int =
    Seq(length + width, width + height, height + length).min * 2

  def volume(length: Int, width: Int, height: Int): Int =
    length * width * height

  def parseInput(input: String): Seq[Seq[Int]] = input.linesIterator.map(_.split('x').map(_.toInt).toSeq).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day02.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(misurePaper(parseInput(input)).last)
    println(misureRibbon(parseInput(input)).last)
  }
}
