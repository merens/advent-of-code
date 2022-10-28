package aoc2016

import org.scalatest.funsuite.AnyFunSuite

class Day07Test extends AnyFunSuite {
  test("count abba in brackets") {
    assert(Day07.countAbbaInBrackets("abba[mnop]qrst").equals(0))
    assert(Day07.countAbbaInBrackets("abcd[bddb]xyyx").equals(1))
    assert(Day07.countAbbaInBrackets("aaaa[qwer]tyui").equals(0))
    assert(Day07.countAbbaInBrackets("ioxxoj[asdfgh]zxcvbn").equals(0))
  }

  test("address support tls") {
    val addresses = Seq(
      "abba[mnop]qrst",
      "abcd[bddb]xyyx",
      "aaaa[qwer]tyui",
      "ioxxoj[asdfgh]zxcvbn"
    )
    assert(Day07.countSupportTls(addresses).equals(2))
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
    assert(Day07.countSupportSsl(addresses).equals(3))
  }

}
