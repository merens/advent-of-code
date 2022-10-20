package aoc2015

import aoc2015.Day10.repeated
import utils.GridImplicits._
import utils.{ Grid, Pos }

object Day18 {

  // Game of Life

  def parseGrid(input: String): Grid[Boolean] =
    input.linesIterator
      .map(_.toVector)
      .toVector
      .map(_.map {
        case '#' => true
        case '.' => false
        case e   => throw new IllegalArgumentException(s"error parsing grid: $e")
      })

  def animate(pos: Pos, grid: Grid[Boolean]): Boolean = {
    val neighbours = Pos.allNeighbours  // alternative: use "empty" borders
      .map(_ + pos)
      .filter(p => grid.containsPos(p))
      .count(grid(_) == true)
    (grid(pos), neighbours) match {
      case (true, 2 | 3) => true
      case (true, _)     => false
      case (false, 3)    => true
      case (false, _)    => false
    }
  }

  def step(grid: Grid[Boolean]): Grid[Boolean] =
    grid.indices
      .map(x => grid.head.indices.map(y => Pos(x, y)))
      .map(_.toVector)
      .toVector
      .map(_.map(animate(_, grid)))
      .transpose // why is it inverted?

  def lightCorners(grid: Grid[Boolean]): Grid[Boolean] = {
    val corners = Seq(Pos(0, 0), Pos(0, grid.size - 1), Pos(grid.head.size - 1, 0), Pos(grid.head.size - 1, grid.size - 1))
    corners.foldLeft(grid)((updated, corner) => updated.updateGrid(corner, true))
  }

  def part2(grid: Grid[Boolean]): Grid[Boolean] = lightCorners(step(grid))

  def printGrid(grid: Grid[Boolean]): Unit =
    grid.indices.foreach { x =>
      grid.head.indices.foreach(y => if (grid(x)(y)) print('#') else print('.'))
      println()
    }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val startingGrid = parseGrid(input)
//    println(printGrid(startingGrid))
    println(repeated(step, 100)(startingGrid).countGrid(_ == true))
    println(repeated(part2, 100)(lightCorners(startingGrid)).countGrid(_ == true))
  }
}
