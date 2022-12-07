package aoc2022

import scala.util.matching.Regex

object Day07 {

  sealed abstract class Tree
  case class Dir(name: String, size: Option[Int], children: Seq[Tree]) extends Tree
  case class File(name: String, size: Int)                             extends Tree

  val directoryRegex: Regex       = """dir (\w+)""".r
  val fileRegex: Regex            = """(\d+) (\w+|\w+\.\w+)""".r
  val changeDirectoryRegex: Regex = """\$ cd (\w+)""".r

  def main(args: Array[String]): Unit = {
    val filledTree = assignSize(parseTree(parseInput(input)))
    println(part1(filledTree))
    println(part2(filledTree))
  }

  def part1(tree: Tree): Int = tree match {
    case d: Dir if d.size.getOrElse(0) <= 100000 => d.size.getOrElse(0) + d.children.map(part1).sum
    case d: Dir                                  => d.children.map(part1).sum
    case _: File                                 => 0
  }

  def part2(tree: Tree): Int = {
    val minSpaceToFree = 30000000 - (70000000 - tree.asInstanceOf[Dir].size.getOrElse(0))
    findMin(tree: Tree, minSpaceToFree)
  }

  def parseTree(instructions: Seq[String]): Tree = {
    val root: Tree        = Dir("/", None, Seq()).asInstanceOf[Tree]
    val startingDirectory = Seq.empty[String]
    instructions
      .foldLeft(
        (root, startingDirectory)
      ) { (status, instruction: String) =>
        val tree = status._1
        val wd   = status._2
        instruction match {
          case changeDirectoryRegex(name)    => (tree, wd :+ name)
          case directoryRegex(directoryName) => (addChild(wd, tree, Dir(directoryName, None, Seq())), wd)
          case fileRegex(size, fileName)     => (addChild(wd, tree, File(fileName, size.toInt)), wd)
          case "$ ls"                        => (tree, wd)
          case "$ cd .."                     => (tree, wd.init)
          case "$ cd /"                      => (tree, wd :+ "/")
          case e                             => throw new Exception(s"parsing error: $e")
        }
      }
      ._1
  }

  def addChild(workingDirectory: Seq[String], parent: Tree, child: Tree): Tree = (parent, workingDirectory) match {
    case (f: File, _)                                             => f
    case (d: Dir, head :: tail) if head == d.name && tail.isEmpty => d.copy(children = d.children :+ child)
    case (d: Dir, head :: tail) if head == d.name                 => d.copy(children = d.children.map(addChild(tail, _, child)))
    case (d: Dir, _)                                              => d
    case e                                                        => throw new Error(s"error adding child: $e")
  }

  def assignSize(tree: Tree): Tree = tree match {
    case d: Dir if d.size.isEmpty   =>
      d.copy(
        size = Some(size(d)),
        children = d.children.map(assignSize)
      )
    case d: Dir if d.size.isDefined => d
    case x                          => x
  }

  def size(tree: Tree): Int = tree match {
    case d: Dir  => d.size.getOrElse(d.children.map(size).sum)
    case f: File => f.size
  }

  def findMin(tree: Tree, minSpaceToFree: Int): Int = tree match {
    case _: File                                => Integer.MAX_VALUE
    case d: Dir if d.size.get >= minSpaceToFree =>
      (d.size.get +: d.children.map(findMin(_, minSpaceToFree))).min
    case d: Dir                                 => d.children.map(findMin(_, minSpaceToFree)).min
  }

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day07.txt")).mkString.trim

}
