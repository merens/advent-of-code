package utils

object GridImplicits {

  implicit class GridOps[A](grid: Grid[A]) {
    def apply(pos: Pos): A = grid(pos.y)(pos.x)

    def containsPos(pos: Pos): Boolean = {
      0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size
    }

    def updateGrid(pos: Pos, elem: A): Grid[A] =
      grid.updated(pos.y, grid(pos.y).updated(pos.x, elem))

    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum
  }
}