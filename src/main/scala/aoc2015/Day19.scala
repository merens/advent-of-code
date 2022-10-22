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

  def replace(s: String, pattern: String, replacement: String): Seq[String] =
    pattern.r
      .findAllMatchIn(s)
      .map(found => (found.start, found.end))
      .map { case (start, end) => s.substring(0, start) + replacement + s.substring(end) }
      .toSeq

  def allReplacements(s: String, map: Map[String, Seq[String]]): Seq[String] =
    map.keySet.toSeq
      .sortBy(_.length)
      .map(key => (key, map(key)))
      .flatMap { case (key, values) =>
        values.map(replace(s, key, _))
      }
      .flatten

  def distinctReplacements(s: String, map: Map[String, Seq[String]]): Seq[String] =
    allReplacements(s, map).distinct

  // part 2
  // greedy? distance from the target string i.e. number of same chars
  // grammar?
  def bottomUp(start: String, end: String, map: Map[String, Seq[String]]): Seq[Int] = {
    println(start)
    if (start.equals(end)) Seq(0)
    else if (start.length > end.length) Seq(10000000)
    else distinctReplacements(start, map).map(bottomUp(_, end, map))
    ???
  }

  // stuck in a loop
  def topDown(string: String, start: String, map: Map[String, Seq[String]]): Int = {
    println(string)
    val replacements = distinctReplacements(string, map)
    println(replacements)
    if (string equals start) 1
    else if (replacements.isEmpty) {
      println(string, "0")
      0
    } else replacements.map(topDown(_, start, map)).max
  }

  // alternative: top-down instead of bottom-up
  // reverse map (_.swap), look for most reducing rules
  def reverseMap(map: Map[String, Seq[String]]): Map[String, Seq[String]] =
    map.foldLeft(Map.empty[String, Seq[String]].withDefaultValue(Seq.empty[String])) {
      (map, tuple: (String, Seq[String])) =>
        tuple._2.foldLeft(map)((map, right) => map + (right -> (map(right) :+ tuple._1)))
    }

  def reverseMap2(map: Map[String, Seq[String]]): Map[String, String]     =
    map.foldLeft(Map.empty[String, String].withDefaultValue("")) { (map, tuple: (String, Seq[String])) =>
      tuple._2.foldLeft(map)((map, right) => map + (right -> tuple._1))
    }

  def countGroups(s: String): Int =
    s.count(_.isUpper)

  def countRn(s: String): Int     =
    "Rn".r.findAllMatchIn(s).size

  def countAr(s: String): Int     =
    "Ar".r.findAllMatchIn(s).size

  def countY(s: String): Int      =
    "Y".r.findAllMatchIn(s).size

  def countSteps(string: String): Int = {
    countGroups(string) - countRn(string) - countAr(string) - 2 * countY(string) - 1
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val (map, string) = parseInput(input)
    val reversed      = reverseMap(map)
    println(distinctReplacements(string, map).size)
    println(countSteps(string))
    //    println(f("e", string, map))
  }

}
