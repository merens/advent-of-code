package utils

class LazyCell[+A](value: => A) {
  lazy val get: A = value
}

object LazyCell {
  def apply[A](value: => A): LazyCell[A] = new LazyCell(value)
}