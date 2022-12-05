package aoc2022

import aoc2022.Day05._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day05Test extends AnyFunSuite {

  val INPUT: String =
    "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

  val CARGOS: Seq[Seq[String]] = Seq(
    Seq("N", "Z"),
    Seq("D", "C", "M"),
    Seq("P")
  )

  val INSTRUCTIONS: Seq[String] = Seq(
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  )

  test("parsing") {
    val parsed                                                      = INPUT.linesIterator.toSeq
    val (cargoInput, instructionsInput): (Seq[String], Seq[String]) = parsed.splitAt(parsed.indexOf(""))
    parseCargo(cargoInput) shouldBe CARGOS
    // splitAt will leave the empty line
    instructionsInput should contain allElementsOf INSTRUCTIONS
  }

  test("step 1") {
    val actual                     = moveCargo(CARGOS, INSTRUCTIONS.head, isReversed = true)
    val expected: Seq[Seq[String]] = Seq(
      Seq("D", "N", "Z"),
      Seq("C", "M"),
      Seq("P")
    )
    actual shouldBe expected
  }

  test("part 1 step 3") {
    val actual                     = moveCargo(CARGOS, INSTRUCTIONS(2), isReversed = true)
    val expected: Seq[Seq[String]] = Seq(
      Seq("C", "D", "N", "Z"),
      Seq("M"),
      Seq("P")
    )
    actual shouldBe expected
  }

  test("part 2 step 3") {
    val actual                     = moveCargo(CARGOS, INSTRUCTIONS(2), isReversed = false)
    val expected: Seq[Seq[String]] = Seq(
      Seq("D", "C", "N", "Z"),
      Seq("M"),
      Seq("P")
    )
    actual shouldBe expected
  }

  test("encoding configuration") {
    val actual   = encodeArrangement(CARGOS)
    val expected = "NDP"
    actual shouldBe expected
  }

  test("part 1 result") {
    val actual   = part1(CARGOS, INSTRUCTIONS)
    val expected = "CMZ"
    actual shouldBe expected
  }

  test("part 2 result") {
    val actual   = part2(CARGOS, INSTRUCTIONS)
    val expected = "MCD"
    actual shouldBe expected
  }

}
