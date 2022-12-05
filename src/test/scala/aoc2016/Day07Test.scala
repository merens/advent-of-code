package aoc2016

import aoc2016.Day07.{ countAbbaInBrackets, countSupportSsl, countSupportTls }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day07Test extends AnyFunSuite {
  test("count abba in brackets") {
    countAbbaInBrackets("abba[mnop]qrst") shouldBe 0
    countAbbaInBrackets("abcd[bddb]xyyx") shouldBe 1
    countAbbaInBrackets("aaaa[qwer]tyui") shouldBe 0
    countAbbaInBrackets("ioxxoj[asdfgh]zxcvbn") shouldBe 0
  }

  test("address support tls") {
    val addresses = Seq(
      "abba[mnop]qrst",
      "abcd[bddb]xyyx",
      "aaaa[qwer]tyui",
      "ioxxoj[asdfgh]zxcvbn"
    )
    countSupportTls(addresses) shouldBe 2
  }

//  test("count aba-bab in brackets") {
//    assert(Day07.hasAbaBab("aba[bab]xyz"))
//    assert(!Day07.hasAbaBab("xyx[xyx]xyx"))
//    assert(Day07.hasAbaBab("aaa[kek]eke"))
//    assert(Day07.hasAbaBab("zazbz[bzb]cdb"))
//  }

  test("address support ssl") {
    val addresses = Seq(
      "aba[bab]xyz",
      "xyx[xyx]xyx",
      "aaa[kek]eke",
      "zazbz[bzb]cdb"
    )
    countSupportSsl(addresses) shouldBe 3
  }

}
