package aoc2016

import utils.GridImplicits._
import utils.{Grid, Pos}

import scala.util.matching.Regex

object Day08 {

  // https://stackoverflow.com/a/32707410
  // I don't think there are cases of by > size but it could be useful
  // TODO: consider using view https://stackoverflow.com/questions/32706319/how-do-you-rotate-circular-shift-of-a-scala-collection#comment53257448_32706860
  def rotateSequenceRight[A](sequence: Seq[A], by: Int): Seq[A] = {
    val size   = sequence.size
    val result = sequence.drop(size - (by % size)) ++ sequence.take(size - (by % size))
    result
  }

  // all the inputs are positive and this wasn't necessary
  def rotateSequenceLeft[A](sequence: Seq[A], by: Int): Seq[A] = {
    val size = sequence.size
    sequence.drop(by % size) ++ sequence.take(by % size)
  }

  def startGrid(): Grid[Boolean] =
    Vector.fill(6)(Vector.fill(50)(false))

  def turnOnTopRectangle(grid: Grid[Boolean], a: Int, b: Int): Grid[Boolean] =
    (0 until a)
      .flatMap(i => (0 until b).map(j => Pos(i, j)))
      .foldLeft(grid)((grid, pos) => grid.updateGrid(pos, true))

  val rectangleRegex: Regex    = """rect (\d+)x(\d+)""".r
  val rotateRowRegex: Regex    = """rotate row y=(\d) by (\d+)""".r
  val rotateColumnRegex: Regex = """rotate column x=(\d+) by (\d+)""".r

  def parseInput(input: String): Grid[Boolean] = input.linesIterator.toSeq
    .foldLeft(startGrid()) { (grid, instruction) =>
      instruction match {
        case rectangleRegex(a, b)     =>
          turnOnTopRectangle(grid, a.toInt, b.toInt)
        case rotateRowRegex(y, by)    =>
          grid.updated(
            y.toInt,
            rotateSequenceRight[Boolean](grid(y.toInt), by.toInt).toVector
          )
        case rotateColumnRegex(x, by) =>
          val transposed = grid.transpose
          transposed
            .updated(x.toInt, rotateSequenceRight[Boolean](transposed(x.toInt), by.toInt).toVector)
            .transpose
        case e                        => throw new IllegalArgumentException(s"error parsing instruction: $e")
      }
    }

  def printPart2(grid: Grid[Boolean]): Unit =
    grid.indices.foreach { x =>
      grid.head.indices.foreach { y =>
        if (y % 5 == 0) print(" ")
        if (grid(x)(y)) print('#') else print(' ')
      }
      println()
    }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day08.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val resultGrid = parseInput(input)
    println(resultGrid.countGrid(_ == true))
    printPart2(resultGrid)
  }
}
