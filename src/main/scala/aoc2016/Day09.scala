package aoc2016

import scala.util.matching.Regex

object Day09 {

  // (NxM)
  val markerRegex: Regex = """\((\d+)x(\d+)\)""".r

  def decompressVersion1(string: String): Int = {
    val marker: Option[Regex.Match] = markerRegex.findFirstMatchIn(string)
    if (marker.isEmpty) string.length
    else {
      val numberOfCharactersToRepeat: Int = marker.get.group(1).toInt
      val timesToRepeat: Int              = marker.get.group(2).toInt
      val markerStartIndex: Int           = marker.get.start
      val markerEndIndex                  = marker.get.end
      markerStartIndex +
        numberOfCharactersToRepeat * timesToRepeat +
        decompressVersion1(string.substring(markerEndIndex + numberOfCharactersToRepeat))
    }
  }

  def decompressVersion2(string: String): Long = {
    val marker: Option[Regex.Match] = markerRegex.findFirstMatchIn(string)
    if (marker.isEmpty) string.length
    else {
      val numberOfCharactersToRepeat: Int = marker.get.group(1).toInt
      val timesToRepeat: Int              = marker.get.group(2).toInt
      val markerStartIndex: Int           = marker.get.start
      val markerEndIndex                  = marker.get.end
      markerStartIndex +
        // this time I decompress the repeated string too
        timesToRepeat.toLong * decompressVersion2(
          string.substring(markerEndIndex, markerEndIndex + numberOfCharactersToRepeat)
        ) +
        decompressVersion2(string.substring(markerEndIndex + numberOfCharactersToRepeat))
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day09.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(decompressVersion1(input))
    println(decompressVersion2(input))
  }
}
