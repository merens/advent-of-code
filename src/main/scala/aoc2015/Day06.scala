package aoc2015

import scala.annotation.tailrec

object Day06 {

  // TODO: better way to handle the matrix of lights
  val startingMap: Map[(Int, Int), Int] = (0 to 999).flatMap(x => (0 to 999).map((x, _) )).map(e => e -> 0).toMap

  // TODO: efficiency? I'm using a lot of recursion and a lot of immutable maps
  @tailrec
  def light(instructions: Seq[String], map: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    if (instructions.isEmpty) map
    else {
      val head = instructions.head
      val remainingInstructions = instructions.drop(1)
      head match {
        case s"turn on $x1,$y1 through $x2,$y2"  => light(remainingInstructions, on(map, x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case s"turn off $x1,$y1 through $x2,$y2" => light(remainingInstructions, off(map, x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case s"toggle $x1,$y1 through $x2,$y2"   => light(remainingInstructions, toggle(map, x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case _                                   => throw new IllegalArgumentException("Parsing failed")
      }
    }

  @tailrec
  def elvishLight(instructions: Seq[String], map: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    if (instructions.isEmpty) map
    else {
      val head = instructions.head
      val remainingInstructions = instructions.drop(1)
      head match {
        case s"turn on $x1,$y1 through $x2,$y2"  => elvishLight(remainingInstructions, on2(map, x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case s"turn off $x1,$y1 through $x2,$y2" => elvishLight(remainingInstructions, off2(map, x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case s"toggle $x1,$y1 through $x2,$y2"   => elvishLight(remainingInstructions, toggle2(map, x1.toInt, y1.toInt, x2.toInt, y2.toInt))
        case _                                   => throw new IllegalArgumentException("Parsing failed")
      }
    }

  def on(map: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Map[(Int, Int), Int]     =
    (x1 to x2).flatMap(x => (y1 to y2).map((x, _) )).scanLeft(map)((map, coordinates) => map + ((coordinates._1, coordinates._2) -> 1)).last

  def off(map: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Map[(Int, Int), Int]    =
    (x1 to x2).flatMap(x => (y1 to y2).map((x, _) )).scanLeft(map)((map, coordinates) => map + ((coordinates._1, coordinates._2) -> 0)).last

  def toggle(map: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Map[(Int, Int), Int] =
    (x1 to x2).flatMap(x => (y1 to y2).map((x, _) )).scanLeft(map)((map, coordinates) => {
      val oldValue = map((coordinates._1, coordinates._2))
      val newValue = if (oldValue == 0) 1 else 0
      map + ((coordinates._1, coordinates._2) -> newValue)
    }).last

  def on2(map: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Map[(Int, Int), Int]     =
  (x1 to x2).flatMap(x => (y1 to y2).map((x, _) )).scanLeft(map)((map, coordinates) => {
      val oldValue = map((coordinates._1, coordinates._2))
      val newValue = oldValue + 1
      map + ((coordinates._1, coordinates._2) -> newValue)
    }).last

  def off2(map: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Map[(Int, Int), Int]    =
  (x1 to x2).flatMap(x => (y1 to y2).map((x, _) )).scanLeft(map)((map, coordinates) => {
      val oldValue = map((coordinates._1, coordinates._2))
      val newValue = Math.max(oldValue - 1, 0)
      map + ((coordinates._1, coordinates._2) -> newValue)
    }).last

  def toggle2(map: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Map[(Int, Int), Int] =
  (x1 to x2).flatMap(x => (y1 to y2).map((x, _) )).scanLeft(map)((map, coordinates) => {
      val oldValue = map((coordinates._1, coordinates._2))
      val newValue = oldValue + 2
      map + ((coordinates._1, coordinates._2) -> newValue)
    }).last


  def parseInput(input: String): Seq[String]                                                      = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day06.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(light(parseInput(input), startingMap).values.sum)
    println(elvishLight(parseInput(input), startingMap).values.sum)
  }
}
