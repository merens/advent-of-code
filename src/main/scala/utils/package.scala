package object utils {
  type Grid[A] = Vector[Vector[A]]

  object Grid {

    def printGrid[A](grid: Grid[A]): Unit =
      grid.foreach { r =>
        r.foreach(print)
        println
      }
  }

  case class Pos(x: Int, y: Int) {
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
    def -(that: Pos): Pos = Pos(x - that.x, y - that.y)

//    https://en.wikipedia.org/wiki/Chebyshev_distance
    def chebyshevDistance(that: Pos): Int =
      Math.max(
        Math.abs(x - that.x),
        Math.abs(y - that.y)
      )
  }

  object Pos {
    lazy val axisNeighbours: Seq[Pos]     = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
    lazy val diagonalNeighbours: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
    lazy val allNeighbours: Seq[Pos]      = axisNeighbours ++ diagonalNeighbours
  }

  type LazyMap[K, +V] = Map[K, LazyCell[V]]
}
