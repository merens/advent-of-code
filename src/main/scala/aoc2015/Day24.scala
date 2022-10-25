package aoc2015

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day24 {

  // partition input
  // optimum weigth distribution?
  // input too big, 28! permutations
  // it's good that they are all the same, we already know how much is the sum of a group
  def parseInput(input: String): Seq[Int] =
    input.linesIterator.toSeq.map(Integer.parseInt)

  // I don't like this: I'm checking up to 6 and not checking if it is possible to create equal groups with the remaining packages
  //  I think the combinations are so much that it's always possible
  def f(packages: Seq[Int], numberOfPartitions: Int): Seq[Seq[Seq[Int]]] = {
    val sum    = packages.sum
    val size   = packages.size
    val target = sum / numberOfPartitions
    (1 to 6).map { n =>
      val shortestSequencesWithTargetSum = packages.combinations(n).filter(_.sum == target).toSeq
      //  this wasn't necessary for part 1 and it's not extensible for part 2
//      shortestSequencesWithTargetSum.filter { l =>
//        val remainingPackages = packages diff l
//        (1 to remainingPackages.size - n).exists(
//          m => remainingPackages.combinations(m).filter(_.sum == target).exists(l2 => (remainingPackages diff l2).sum == target)
//        )
//      }
      shortestSequencesWithTargetSum
    }.filter(_.nonEmpty)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val packages = parseInput(input)
    val result = f(packages, 3).map(_.map(_.map(_.toLong)))
    println(result.flatMap(_.map(_.product).sorted).min)
    println(f(packages, 4).map(_.map(_.map(_.toLong))).flatMap(_.map(_.product).sorted).min)
  }
}
