package utils

object GridImplicits {

  // from https://github.com/sim642/adventofcode/blob/master/src/main/scala/eu/sim642/adventofcodelib/GridImplicits.scala
  implicit class GridOps[A](grid: Grid[A]) {
    def apply(pos: Pos): A = grid(pos.x)(pos.y)

    def containsPos(pos: Pos): Boolean = {
      0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size
    }

    def updateGrid(pos: Pos, elem: A): Grid[A] =
      grid.updated(pos.x, grid(pos.x).updated(pos.y, elem))

    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum
  }
}