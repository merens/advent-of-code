package aoc2015

object Day19 {

  def parseInput(input: String): (Map[String, Seq[String]], String) = {

    val lines: Vector[String]         = input.linesIterator.toVector
    val startingPoint                 = lines.last
    val map: Map[String, Seq[String]] = lines
      .foldLeft(Map.empty[String, Seq[String]].withDefaultValue(Seq.empty[String])) { (map, row: String) =>
        row match {
          case s"$left => $right" => map + (left -> (map(left) :+ right))
          case _: String          => map
          case e                  => throw new IllegalArgumentException(s"error parsing input: $e")
        }
      }
    (map, startingPoint)
  }

  def generate(s: String, pattern: String, replacement: String): Seq[String] =
    pattern.r
      .findAllMatchIn(s)
      .map(found => (found.start, found.end))
      .map { case (start, end) => s.substring(0, start) + replacement + s.substring(end) }
      .toSeq

  def allReplacements(s: String, map: Map[String, Seq[String]]): Seq[String] = {
    map.keySet.toSeq
      .map(key => (key, map(key)))
      .flatMap { case (key, values) =>
        values.map(generate(s, key, _))
      }
      .flatten
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val (map, string) = parseInput(input)
    println(allReplacements(string, map).distinct.size)
  }
}
