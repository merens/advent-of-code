package aoc2016

import scala.util.matching.Regex

object Day04 {

  def isNotDecoy(code: String, expectedChecksum: String): Boolean = {
    val actualChecksum = code
      .replaceAll("-", "")
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq
      // interesting way to combine sorts priority:
      // decreasing order for size of the frequency first, then lexicographical for char
      .sortBy(t => (-t._2, t._1))
      .take(5)
      .map(_._1)
      .mkString
    actualChecksum.equals(expectedChecksum)
  }

  val roomRegex: Regex = """([a-z-]+)+(\d+)\[(\w+)]+""".r

  def filterDecoyRooms(lines: Seq[String]): Seq[Int] =
    lines
      .map {
        case roomRegex(code, sector, checksum) => if (isNotDecoy(code, checksum)) sector.toInt else 0
        case e                                 => throw new IllegalArgumentException(s"error: $e")
      }

  def decrypt(string: String, sector: Int): String = {
    val map: Map[Char, Char]           = ('a' to 'y').foldLeft(Map.empty[Char, Char])((map, char) =>
      map + (char -> (char + 1).toChar)
    ) + ('z' -> 'a') + ('-' -> ' ') + (' ' -> ' ')
    def cypher(string: String): String = string.map(char => map(char))
    aoc2015.Day10.repeated(
      cypher,
      sector
    )(string)
  }

  // similar to other filter, but used flatmap to filter decoys
  def filterAndDecrypt(lines: Seq[String]): Seq[(String, Int)] =
    lines.flatMap {
      case roomRegex(code, sector, checksum) =>
        if (isNotDecoy(code, checksum)) {
          val decryptedRoomName: String = decrypt(code, sector.toInt)
          Some(decryptedRoomName, sector.toInt)
        } else None
      case e                                 => throw new IllegalArgumentException(s"error: $e")
    }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day04.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(filterDecoyRooms(parseInput(input)).sum)
    println(
      filterAndDecrypt(parseInput(input))
        .filter(t => t._1.startsWith("north"))
    )
  }
}
