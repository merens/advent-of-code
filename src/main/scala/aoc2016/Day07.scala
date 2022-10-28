package aoc2016

import scala.util.matching.Regex

object Day07 {

//  https://stackoverflow.com/questions/61971565/c-sharp-regular-expression-abba#comment109947962_61971646
  val abbaRegex: Regex             = """(.)(.)\2(?!\2)\1""".r
  val containsAbbaRegex: Regex     = s""".*$abbaRegex.*""".r
  val hypernetSequenceRegex: Regex = """\[(\w+)]""".r

  def countAbbaInBrackets(ipv7address: String): Int =
    hypernetSequenceRegex
      .findAllIn(ipv7address)
      .map {
        case containsAbbaRegex(_, _) => true
        case _                       => false
      }
      .count(_ == true)

  def countSupportTls(addresses: Seq[String]): Int =
    addresses
      .map { address =>
        if (countAbbaInBrackets(address) > 0) false
        else if (abbaRegex.findAllMatchIn(address).nonEmpty) true
        else false
      }
      .count(_ == true)

  def hasAbaBab(hypernet: String, abaGroups: Seq[String]): Boolean =
    abaGroups.exists { aba =>
      val b           = aba.tail.head
      val a           = aba.head
      val bab: String = Seq(b, a, b).mkString
      hypernet.contains(bab)
    }

  def abaGroups(supernet: String): Seq[String] =
    supernet
      // maybe it's easier without regex :|
      .sliding(3)
      .filter(xyz => xyz(0) == xyz(2) && xyz(0) != xyz(1))
      .toSeq

  def countSupportSsl(addresses: Seq[String]): Int =
    addresses.count { address =>
      val supernet: String       = address.replaceAll(hypernetSequenceRegex.toString, " ")
      val hypernets: Seq[String] = hypernetSequenceRegex.findAllMatchIn(address).map(_.group(0)).toSeq
      val aba                    = abaGroups(supernet)
      hypernets.exists(hasAbaBab(_, aba))
    }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day07.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countSupportTls(parseInput(input)))
    println(countSupportSsl(parseInput(input)))
  }
}
