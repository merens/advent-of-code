package aoc2015

object Day17 {

  // backpack problem?
  //  https://en.wikipedia.org/wiki/Knapsack_problem

  def parseContainers(s: String): Seq[Int] =
    s.linesIterator.toSeq.map(_.toInt)

  def containerCombinations(containers: Seq[Int]): Seq[Seq[Int]] =
    (1 to containers.size)
      // combinations ignores repetitions, so I zip with the index to distinguish duplicates
      .flatMap(containers.zipWithIndex.combinations)
      .map(_.map(_._1))

  def numberOfCombinationsWithSum(sum: Int)(containers: Seq[Int]): Int =
    containerCombinations(containers).count(_.sum == sum)

  def numberOfCombinationsMinsWithSum(sum: Int)(containers: Seq[Int]): Int = {
    val filteredCombinations = containerCombinations(containers).filter(_.sum == sum)
    val minWithSum: Int      = filteredCombinations
      .map(_.size)
      .min
    filteredCombinations.count(_.size == minWithSum)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(numberOfCombinationsWithSum(150)(parseContainers(input)))
    println(numberOfCombinationsMinsWithSum(150)(parseContainers(input)))
  }
}
