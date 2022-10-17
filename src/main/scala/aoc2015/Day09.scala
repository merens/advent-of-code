package aoc2015

object Day09 {

  case class WeightedGraph(nodes: Set[Node]){
    override def toString: String = nodes.map(n => n.city + " " + n.neighbours).mkString("\n")
  }

  case class Node(city: String, neighbours: Map[String, Int] = Map.empty)

  def addNode(graph: WeightedGraph, from: String, to: String, n: Int): WeightedGraph = {
    if (graph.nodes.map(_.city).contains(from)) {
      val oldNode = graph.nodes.filter(_.city == from).head
      val newNode = oldNode.copy(neighbours = oldNode.neighbours + (to -> n))
      graph.copy(nodes = graph.nodes - oldNode + newNode)
    }
    else {
      val newNode = Node(from, Map(to -> n))
      graph.copy(nodes = graph.nodes + newNode)
    }
    // consider adding the to to the set of nodes
    // consider using only one class Graph(city, map(from, to) -> int) DONE
  }

  def createGraph(rows: Seq[String]): WeightedGraph =
    rows
      .foldLeft(WeightedGraph(Set.empty)) { (graph, row: String) =>
        row match {
          case s"$from to $to = $n" => addNode(graph, from, to, n.toInt)
          case _                    => throw new IllegalArgumentException("parsing error")
        }
      }
  // parse input
  // save graph info:  map or list of neighbours for every node? and the cost?
  //  www.scala-graph.org seems interesting
  // optimal path

  // greedy? shortest weigthed path of a grapth, no start or end point
  // maybe Dijkstra’s Algorithm from every point?

  // alternative from sim642: permutate all paths and calculate all distances
  type Graph[A] = Map[A, Map[A, Int]]

  def createGraph2(rows: Seq[String]): Graph[String] =
    rows
      .scanLeft(Map.empty[String, Map[String, Int]].withDefaultValue(Map.empty[String, Int])) { (graph, row: String) =>
        row match {
          case s"$from to $to = $n" =>
            graph +
              (from -> (graph(from) + (to -> n.toInt))) +
              (to -> (graph(to) + (from -> n.toInt)))   // it's a bidirectional graph
          case _ => throw new IllegalArgumentException("parsing error")
        }
      }
      .last

  def allPaths(g: Graph[String]): Seq[Seq[String]] =
    g.keySet.toVector.permutations.toSeq

  def pathLength(g: Graph[String], paths: Seq[String]): Int =
    paths.sliding(2)
      .toSeq
      .map{
        case Vector(from, to) => g(from)(to)
      }
      .sum

  def shortestPath(g: Graph[String], paths: Seq[Seq[String]]): Int =
    paths.map(pathLength(g, _)).min

  def longestPath(g: Graph[String], paths: Seq[Seq[String]]): Int =
    paths.map(pathLength(g, _)).max

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day09.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val graph = createGraph2(parseInput(input))
    println(shortestPath(graph, allPaths(graph)))
    println(longestPath(graph, allPaths(graph)))
  }
}
