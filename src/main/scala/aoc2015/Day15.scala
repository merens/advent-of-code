package aoc2015

import scala.collection.mutable.ListBuffer

object Day15 {

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  def parseInput(input: String): Seq[Ingredient] = input.linesIterator.toSeq
    .map {
//      Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
      case s"$name: capacity $capacity, durability $durability, flavor $flavor, texture $texture, calories $calories" =>
        Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
      case _                                                                                                          => throw new IllegalArgumentException("parsing error")
    }

  def calculateScore(ingredients: Seq[Ingredient], teaspoons: Seq[Int]): Int = {
    val temp = ingredients.zip(teaspoons).map { case (ingredient, teaspoon) =>
      (
        teaspoon * ingredient.capacity,
        teaspoon * ingredient.durability,
        teaspoon * ingredient.flavor,
        teaspoon * ingredient.texture
      )
    }
    // I don't like this, alternatives?
    // from the tests I see big numbers, Long? BigInt? no need
    Math.max(0, temp.map(_._1).sum) *
      Math.max(0, temp.map(_._2).sum) *
      Math.max(0, temp.map(_._3).sum) *
      Math.max(0, temp.map(_._4).sum)
  }

  def teaspoonCombinations(sum: Int)(numberOfIngredients: Int): Seq[Seq[Int]] = {
    val lb = new ListBuffer[Seq[Int]]
    for (a <- 0 until sum)
      for (b <- 0 until sum)
        for (c <- 0 until sum)
          // I know that there are 4 ingredients so I stop at 'd', how can I use the number of ingredients to do 'numberOfIngredients' nested for loops?
          for (d <- 0 until sum)
            if (a + b + c + d == sum)
              lb.append(Seq(a, b, c, d))
    lb.toSeq
  }

  def calculateCalories(ingredients: Seq[Ingredient], teaspoons: Seq[Int]): Int =
    ingredients.zip(teaspoons).map { case (ingredient, teaspoon) => ingredient.calories * teaspoon }.sum

  lazy val input: String                                                        = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val ingredients: Seq[Ingredient] = parseInput(input)
    println(ingredients)
    println(
      teaspoonCombinations(100)(ingredients.size)
        .map(calculateScore(ingredients, _))
        .max
    )
    println(
      teaspoonCombinations(100)(ingredients.size)
        .filter(calculateCalories(ingredients, _) == 500)
        .map(calculateScore(ingredients, _))
        .max
    )
  }
}
