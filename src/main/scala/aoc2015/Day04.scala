package aoc2015
import java.security.MessageDigest

object Day04 {

  def bruteforce(secretKey: String, prefix: String):Int = {
    (0 to Int.MaxValue).foreach(i => {
      val hash = md5(secretKey + i)
      val hex = hash.map(b => String.format("%02x", Byte.box(b))).mkString
      if(hex.startsWith(prefix)) {
        println(hex)
        return i
      }
    }
    )
    -1
  }

  def md5(s: String): Array[Byte] = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day04.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bruteforce(input, "00000"))
    println(bruteforce(input, "000000"))
  }
}


