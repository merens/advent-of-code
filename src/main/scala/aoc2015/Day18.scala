package aoc2015

import aoc2015.Day10.repeated
import utils.GridImplicits._
import utils.{ Grid, Pos }

object Day18 {

  // Game of Life

//  case class Grid(config: Seq[Seq[Int]]){
//    override def toString: String = config.mkString("\n")
//  }

//  def neighbours(x: Int, y: Int): Seq[(Int, Int)] =
//    (-1 to 1)
//      .flatMap(a => (-1 to 1)
//      .map(b => if (a == b && a == 0) None else Some(a, b) ))
//      .flatten

  def parseGrid(input: String): Grid[Boolean] =
    input.linesIterator
      .map(_.toVector)
      .toVector
      .map(_.map {
        case '#' => true
        case '.' => false
        case e   => throw new IllegalArgumentException(s"error parsing grid: $e")
      })

//  def addFrame(g: Grid): Grid = {
//    val row: Seq[Int] = Seq.fill(g.config.head.size + 2)(-1)
//    Grid(
//      g.config
//        .map(_.prepended(-1).appended(-1))
//        .prepended(row)
//        .appended(row)
//    )
//  }
  def animate(pos: Pos, grid: Grid[Boolean]): Boolean = {
    val neighbours = Pos.allNeighbours
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
      .transpose

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
  }
}
