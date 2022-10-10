package aoc2015

object Day03 {

  def mapHouses(directions: String): Seq[(Int, Int)] =
    directions
      .scanLeft((0, 0)) { (location, direction) =>
        direction match {
          case '^' => (location._1, location._2 + 1)
          case '>' => (location._1 + 1, location._2)
          case '<' => (location._1 - 1, location._2)
          case 'v' => (location._1, location._2 - 1)
          case _   => throw new IllegalArgumentException("illegal direction")
        }
      }

  def mapRoboSanta(directions: String): Int = {
    val (oddDirections, evenDirections) = directions.toVector.zipWithIndex.partition(_._2 % 2 == 0)
    val santaPath                       = oddDirections.map(_._1).mkString
    val roboPath                        = evenDirections.map(_._1).mkString
    Seq(mapHouses(santaPath), mapHouses(roboPath)).flatten.distinct.size
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day03.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(mapHouses(input).distinct.size)
    println(mapRoboSanta(input))
  }
}
