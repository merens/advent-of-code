package aoc2016

import org.scalatest.funsuite.AnyFunSuite
import utils.Pos

class Day04Test extends AnyFunSuite {
  test("decoy") {
    assert(Day04.isNotDecoy("aaaaa-bbb-z-y-x", "abxyz"))
    assert(Day04.isNotDecoy("a-b-c-d-e-f-g-h", "abcde"))
    assert(Day04.isNotDecoy("not-a-real-room", "oarel"))
    assert(!Day04.isNotDecoy("totally-real-room", "decoy"))
  }

  test("regex") {
    assert(Day04.roomRegex.findAllMatchIn("aaaaa-bbb-x-y-z-200[abxyz]").size == 1)
    assert(Day04.roomRegex.findAllMatchIn("a-b-c-d-e-f-g-h-987[abcde]").size == 1)
    assert(Day04.roomRegex.findAllMatchIn("not-a-real-room-404[oarel]").size == 1)
    assert(Day04.roomRegex.findAllMatchIn("totally-real-room-200[decoy]").size == 1)
  }

  test("sum") {
    assert(Day04.filterDecoyRooms(Seq("aaaaa-bbb-x-y-z-200[abxyz]")).equals(Seq(200)))
  }

  test("decrypt") {
    assert(Day04.decrypt("qzmt-zixmtkozy-ivhz-", 343).equals("very encrypted name "))
  }

}
