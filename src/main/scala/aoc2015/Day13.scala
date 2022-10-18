package aoc2015

// TODO consider putting this in a library
import Day09.Graph

object Day13 {

  // is there a circular list type in Scala?
  // seems similar to 2012 day 09, the one with the cities: I have a list of persons, each with two neighbours and a variable cost
  //  parse input
  //  possible to calculate all permutations of the list and calculate the happiness for each person
  //  choose the best: sum of happiness is higher

  def createGraph(input: String): Graph[String] =
    input.linesIterator.toSeq
      .foldLeft(Map.empty[String, Map[String, Int]].withDefaultValue(Map.empty[String, Int])) { (graph, row: String) =>
        row match {
//        Alice would lose 2 happiness units by sitting next to Bob.
          case s"$person would lose $n happiness units by sitting next to $neighbour." =>
            graph + (person -> (graph(person) + (neighbour -> -n.toInt)))
//        Alice would gain 65 happiness units by sitting next to David.
          case s"$person would gain $n happiness units by sitting next to $neighbour." =>
            graph + (person -> (graph(person) + (neighbour -> n.toInt)))
          case _                                                                       => throw new IllegalArgumentException("parsing error")
        }
      }

  // these arrangement methods are similar to the path ones in Day09, library?
  def allArrangements(g: Graph[String]): Seq[Seq[String]] =
    g.keySet.toVector.permutations.toSeq

  def arrangementHappiness(g: Graph[String], arrangement: Seq[String]): Int = {
    val rightNeighbours: Seq[String]    = arrangement.tail :+ arrangement.head
    val leftNeighbours: Seq[String]     = arrangement.takeRight(1) ++: arrangement.dropRight(1)
    val allPairs: Seq[(String, String)] = Seq(arrangement.zip(rightNeighbours), arrangement.zip(leftNeighbours)).flatten
    allPairs.map { case (person, neighbour) =>
      g(person)(neighbour)
    }.sum
  }

  def happiestArrangement(g: Graph[String], arrangements: Seq[Seq[String]]): Int =
    arrangements.map(arrangementHappiness(g, _)).max

  def addApathethic(g: Graph[String]): Graph[String] =
    g.keySet.foldLeft(g.withDefaultValue(Map.empty[String, Int])) { (graph, person: String) =>
      val myName: String = "me"
      graph + (myName -> (graph(myName) + (person -> 0))) + (person -> (graph(person) + (myName -> 0)))
    }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val g: Graph[String]    = createGraph(input)
    println(happiestArrangement(g, allArrangements(g)))
    val newG: Graph[String] = addApathethic(g)
    // Is there a way to use this functions without passing the graph as parameter two times?
    println(happiestArrangement(newG, allArrangements(newG)))
  }
}
