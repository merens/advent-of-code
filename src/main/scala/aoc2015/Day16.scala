package aoc2015

object Day16 {

  case class Sue(id: Int, properties: Map[String, Int])
// MCFSAM output
//  children: 3
//  cats: 7
//  samoyeds: 2
//  pomeranians: 3
//  akitas: 0
//  vizslas: 0
//  goldfish: 5
//  trees: 3
//  cars: 2
//  perfumes: 1

  // is optional parsing with regex possible?
  def parseInput(input: String): Seq[Sue] = input.linesIterator.toSeq
    .map {
//      Sue 1: children: 1, cars: 8, vizslas: 7
      case s"Sue $id: $propertyList" =>
        val properties: Map[String, Int] = propertyList
          .split(",")
          .toSeq
          .map(_.trim)
          .foldLeft(Map.empty[String, Int].withDefaultValue(-1)) {
            case (m, s"children: $n")    => m + ("children"    -> n.toInt)
            case (m, s"cats: $n")        => m + ("cats"        -> n.toInt)
            case (m, s"samoyeds: $n")    => m + ("samoyeds"    -> n.toInt)
            case (m, s"pomeranians: $n") => m + ("pomeranians" -> n.toInt)
            case (m, s"akitas: $n")      => m + ("akitas"      -> n.toInt)
            case (m, s"vizslas: $n")     => m + ("vizslas"     -> n.toInt)
            case (m, s"goldfish: $n")    => m + ("goldfish"    -> n.toInt)
            case (m, s"trees: $n")       => m + ("trees"       -> n.toInt)
            case (m, s"cars: $n")        => m + ("cars"        -> n.toInt)
            case (m, s"perfumes: $n")    => m + ("perfumes"    -> n.toInt)
            case e                       => throw new IllegalArgumentException(s"error parsing properties: $e")
          }
//        println(properties)
        Sue(id.toInt, properties)
      case _                         => throw new IllegalArgumentException("error parsing Sue")
    }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(
      parseInput(input)
        .filter(sue => sue.properties("children") == 3 || sue.properties("children") < 0)
        .filter(sue => sue.properties("cats") == 7 || sue.properties("cats") < 0)
        .filter(sue => sue.properties("samoyeds") == 2 || sue.properties("samoyeds") < 0)
        .filter(sue => sue.properties("pomeranians") == 3 || sue.properties("pomeranians") < 0)
        .filter(sue => sue.properties("akitas") == 0 || sue.properties("akitas") < 0)
        .filter(sue => sue.properties("vizslas") == 0 || sue.properties("vizslas") < 0)
        .filter(sue => sue.properties("goldfish") == 5 || sue.properties("goldfish") < 0)
        .filter(sue => sue.properties("trees") == 3 || sue.properties("trees") < 0)
        .filter(sue => sue.properties("cars") == 2 || sue.properties("cars") < 0)
        .find(sue => sue.properties("perfumes") == 1 || sue.properties("perfumes") < 0)
        .map(_.id)
    )

    println(
      parseInput(input)
        .filter(sue => sue.properties("children") == 3 || sue.properties("children") < 0)
        .filter(sue => sue.properties("cats") > 7 || sue.properties("cats") < 0)
        .filter(sue => sue.properties("samoyeds") == 2 || sue.properties("samoyeds") < 0)
        .filter(sue => sue.properties("pomeranians") < 3)
        .filter(sue => sue.properties("akitas") == 0 || sue.properties("akitas") < 0)
        .filter(sue => sue.properties("vizslas") == 0 || sue.properties("vizslas") < 0)
        .filter(sue => sue.properties("goldfish") < 5)
        .filter(sue => sue.properties("trees") > 3 || sue.properties("trees") < 0)
        .filter(sue => sue.properties("cars") == 2 || sue.properties("cars") < 0)
        .find(sue => sue.properties("perfumes") == 1 || sue.properties("perfumes") < 0)
        .map(_.id)
    )
  }
}
