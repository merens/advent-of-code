package aoc2015

import org.json4s._
import org.json4s.jackson.JsonMethods._

object Day12 {

  def parseInts(s: String): Seq[Int] = {
    println(s)
    "-?\\d+".r.findAllIn(s.replaceAll("\"", "")).map(_.toInt).toSeq
  }

  // could not find a pattern match to do what I need

  // alternative: parse json and work with dictionaries
  //  https://www.reddit.com/r/adventofcode/comments/pffhut/2015_day_12_part_2_python_i_am_trying_to/hb461u6/
  // problems: default Number type is BigInt instead of Int, the alternative is Long
  //  you can pass a parameter useBigIntForLong in parse(): https://stackoverflow.com/a/53300338
  def parseJson(jsonStr: String): Map[String, Any] = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    parse(jsonStr).extract[Map[String, Any]]
  }

  // list can also contains maps, in the end I have two recursive functions that can call each other
  def excludeRedElements(l: Seq[Any]): Seq[Any] = {
    val strings: Seq[String]        = l.filter(_.isInstanceOf[String]).map(_.asInstanceOf[String])
    val ints: Seq[BigInt]           = l.filter(_.isInstanceOf[BigInt]).map(_.asInstanceOf[BigInt])
    val maps: Seq[Map[String, Any]] = l.filter(_.isInstanceOf[Map[String, Any]]).map(_.asInstanceOf[Map[String, Any]])
    val lists: Seq[Seq[Any]]        = l.filter(_.isInstanceOf[Seq[Any]]).map(_.asInstanceOf[Seq[Any]])
    Seq(ints, strings, maps.map(excludeRedElements), lists.map(excludeRedElements))
  }

  // is there a way to simplify all this type checking and conversion?
  //  one possible ref: https://stackoverflow.com/questions/10323725/scala-short-and-type-safe-cast-operator
  //  maybe pattern match on the type of the elements?
  def excludeRedElements(m: Map[String, Any]): Seq[Any] = {
    val stringProperties: Seq[String] = m.values.filter(_.isInstanceOf[String]).toSeq.map(_.asInstanceOf[String])
    if (stringProperties.contains("red")) Seq.empty[BigInt]
    else {
      val intProperties: Seq[BigInt]                = m.values.filter(_.isInstanceOf[BigInt]).toSeq.map(_.asInstanceOf[BigInt])
      // need to be careful with List that cointain Maps
      val arrayProperties: Seq[Any]                 = m.values.filter(_.isInstanceOf[List[Any]]).toSeq
      val arrayStringProperties: Seq[String]        =
        arrayProperties.filter(_.isInstanceOf[String]).map(_.asInstanceOf[String])
      val arrayIntProperties: Seq[BigInt]           = arrayProperties.filter(_.isInstanceOf[BigInt]).map(_.asInstanceOf[BigInt])
      // another problem: list of lists
      val arrayMapProperties: Seq[Map[String, Any]] =
        arrayProperties.filter(_.isInstanceOf[Map[String, Any]]).map(_.asInstanceOf[Map[String, Any]])
      val arrayListProperties: Seq[Seq[Any]]        =
        arrayProperties.filter(_.isInstanceOf[Seq[Any]]).map(_.asInstanceOf[Seq[Any]])
      val mapProperties: Seq[Map[String, Any]]      =
        m.values.filter(_.isInstanceOf[Map[String, Any]]).toSeq.map(_.asInstanceOf[Map[String, Any]])
      Seq(
        intProperties,
        arrayStringProperties,
        arrayIntProperties,
        arrayListProperties.map(excludeRedElements),
        arrayMapProperties.map(excludeRedElements),
        mapProperties.map(excludeRedElements)
      )
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(parseInts(input).sum)
    println(parseInts(excludeRedElements(parseJson(input)).toString).sum)
  }
}
