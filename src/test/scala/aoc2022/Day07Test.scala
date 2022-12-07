package aoc2022

import aoc2022.Day07._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day07Test extends AnyFunSuite {

  val INPUT: String =
    "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"

  val FILESYSTEM: Tree =
    Dir(
      "/",
      None,
      Seq(
        Dir(
          "a",
          None,
          Seq(
            Dir("e", None, Seq(File("i", 584))),
            File("f", 29116),
            File("g", 2557),
            File("h.lst", 62596)
          )
        ),
        File("b.txt", 14848514),
        File("c.dat", 8504156),
        Dir(
          "d",
          None,
          Seq(
            File("j", 4060174),
            File("d.log", 8033020),
            File("d.ext", 5626152),
            File("k", 7214296)
          )
        )
      )
    )

  test("parsing") {
    parseTree(INPUT.linesIterator.toSeq) shouldBe FILESYSTEM
  }

  test("part 1 result") {
    val actual   = part1(assignSize(FILESYSTEM))
    val expected = 95437
    actual shouldBe expected
  }

  test("part 2 result") {
    val actual   = part2(assignSize(FILESYSTEM))
    val expected = 24933642
    actual shouldBe expected
  }
}
