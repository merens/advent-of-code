package aoc2022

import utils.GridImplicits._
import utils.{ Grid, Pos }

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day09 {

  def main(args: Array[String]): Unit = {
    val startPosH: Pos  = Pos(SIZE / 2, SIZE / 2)
    val startPosT: Pos  = startPosH
    println(part1(startingGrid, parseInput(input), startPosH, startPosT))
    val knots: Seq[Pos] = Seq.fill(10)(0).map(_ => startPosH)
    println(part2(startingGrid, parseInput(input), knots))
  }

  // grid of booleans(visited by T or not) (0, 0) at top left
  // save position of T and H in two Pos

  // assume starting grid 100x100 and start from 50, 50
  private val SIZE                = 500
  // what happens when out of bounds? used a const and increased up to 500
  // TODO: think of a way to increase the size of the grid and keep the knots coordinates valid
  val startingGrid: Grid[Boolean] = Vector.fill(SIZE, SIZE)(false).updateGrid(Pos(SIZE / 2, SIZE / 2), true)
  val directionsRegex: Regex      = """([URDL]) (\d+)""".r

  // this is a special case of updateKnots
  def updateTail(headPos: Pos, tailPos: Pos, d: String): Pos =
    if (headPos.chebyshevDistance(tailPos) <= 1) tailPos
    else
      d match {
        case "U" => headPos.copy(x = headPos.x + 1)
        case "R" => headPos.copy(y = headPos.y - 1)
        case "D" => headPos.copy(x = headPos.x - 1)
        case "L" => headPos.copy(y = headPos.y + 1)
      }

  @tailrec
  def moveTwoKnots(
    direction: String,
    n: Int,
    headPos: Pos,
    tailPos: Pos,
    grid: Grid[Boolean]
  ): (Grid[Boolean], Pos, Pos) =
    if (n == 0)
      (grid, headPos, tailPos)
    else {
      val updatedHeadPos: Pos        = direction match {
        case "U" => headPos.copy(x = headPos.x - 1)
        case "R" => headPos.copy(y = headPos.y + 1)
        case "D" => headPos.copy(x = headPos.x + 1)
        case "L" => headPos.copy(y = headPos.y - 1)
        case e   => throw new IllegalArgumentException(s"error parsing $e")
      }
      val updatedTailPos: Pos        = updateTail(updatedHeadPos, tailPos, direction)
      val updatedGrid: Grid[Boolean] = grid.updateGrid(updatedTailPos, true)
      moveTwoKnots(direction, n - 1, updatedHeadPos, updatedTailPos, updatedGrid)
    }

  def part1(
    startingGrid: Grid[Boolean],
    instructions: Seq[String],
    startingHeadPosition: Pos,
    startingTailPosition: Pos
  ): Int =
    instructions
      .foldLeft((startingGrid, startingHeadPosition, startingTailPosition)) { (status, instruction: String) =>
        val grid: Grid[Boolean] = status._1
        val headPos: Pos        = status._2
        val tailPos: Pos        = status._3
        instruction match {
          case directionsRegex(d, n) => moveTwoKnots(d, n.toInt, headPos, tailPos, grid)
          case e                     => throw new IllegalArgumentException(s"error parsing $e")
        }
      }
      ._1
      .countGrid(_ == true)

  def updateKnots(newHeadPos: Pos, knots: Seq[Pos]): Seq[Pos] =
    if (knots.isEmpty) Nil
    else if (newHeadPos.chebyshevDistance(knots.head) <= 1) knots
    else {
      val movedKnotPos: Pos = {
        val distance: Pos = newHeadPos - knots.head
        // https://www.reddit.com/r/adventofcode/comments/zgnice/2022_day_9_solutions/izhzxb6/
        knots.head + distance.sign
      }
      movedKnotPos +: updateKnots(movedKnotPos, knots.tail)
    }

  @tailrec
  def moveKnots(direction: String, n: Int, knots: Seq[Pos], grid: Grid[Boolean]): (Grid[Boolean], Seq[Pos]) =
    if (n == 0)
      (grid, knots)
    else {
      val updatedHeadPos: Pos        = direction match {
        case "U" => knots.head.copy(x = knots.head.x - 1)
        case "R" => knots.head.copy(y = knots.head.y + 1)
        case "D" => knots.head.copy(x = knots.head.x + 1)
        case "L" => knots.head.copy(y = knots.head.y - 1)
        case e   => throw new IllegalArgumentException(s"error parsing $e")
      }
      val updatedKnotsPos: Seq[Pos]  = updateKnots(updatedHeadPos, knots.tail)
      val updatedGrid: Grid[Boolean] = grid.updateGrid(updatedKnotsPos.last, true)
      moveKnots(direction, n - 1, updatedHeadPos +: updatedKnotsPos, updatedGrid)
    }

  // TODO: better visualization
  def printGrid(grid: Grid[Boolean], knotsPos: Seq[Pos]): Unit = {
    val set = knotsPos.distinct
    grid.indices
      .flatMap(x =>
        grid.head.indices
          .map(Pos(x, _))
      )
      .foreach { p =>
        if (p.y == 0) println
        p match {
          case p: Pos if set.contains(p) => print(set.indexOf(p))
          case _                         => print(".")
        }
      }
    println
  }

  def part2(startingGrid: Grid[Boolean], instructions: Seq[String], knotsStartingPos: Seq[Pos]): Int =
    instructions
      .foldLeft((startingGrid, knotsStartingPos)) { (status, instruction: String) =>
        val grid: Grid[Boolean] = status._1
        val knotsPos: Seq[Pos]  = status._2
        instruction match {
          case directionsRegex(d, n) => moveKnots(d, n.toInt, knotsPos, grid)
          case e                     => throw new IllegalArgumentException(s"error parsing $e")
        }
      }
      ._1
      .countGrid(_ == true)

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day09.txt")).mkString.trim
}
