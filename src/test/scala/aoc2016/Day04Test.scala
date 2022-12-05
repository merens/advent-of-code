package aoc2016

import aoc2016.Day04.{ decrypt, filterDecoyRooms, isNotDecoy, roomRegex }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day04Test extends AnyFunSuite {
  test("decoy") {
    assert(isNotDecoy("aaaaa-bbb-z-y-x", "abxyz"))
    assert(isNotDecoy("a-b-c-d-e-f-g-h", "abcde"))
    assert(isNotDecoy("not-a-real-room", "oarel"))
    assert(!isNotDecoy("totally-real-room", "decoy"))
  }

  test("regex") {
    roomRegex.findAllMatchIn("aaaaa-bbb-x-y-z-200[abxyz]").size shouldBe 1
    roomRegex.findAllMatchIn("a-b-c-d-e-f-g-h-987[abcde]").size shouldBe 1
    roomRegex.findAllMatchIn("not-a-real-room-404[oarel]").size shouldBe 1
    roomRegex.findAllMatchIn("totally-real-room-200[decoy]").size shouldBe 1
  }

  test("sum") {
    filterDecoyRooms(Seq("aaaaa-bbb-x-y-z-200[abxyz]")) shouldBe Seq(200)
  }

  test("decrypt") {
    decrypt("qzmt-zixmtkozy-ivhz-", 343) shouldBe "very encrypted name "
  }

}
