package object utils {
  type Grid[A] = Vector[Vector[A]]

  type LazyMap[K, +V] = Map[K, LazyCell[V]]
}
