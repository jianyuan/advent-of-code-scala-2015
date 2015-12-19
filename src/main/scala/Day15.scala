import scala.collection.SortedMap
import scala.io.Source

/**
  * Created by Jian Yuan on 19/12/15.
  */
object Day15 extends App {
  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
    def toStats(quantity: Int) = Seq(capacity, durability, flavor, texture).map(_ * quantity)

    def toStatsWithCalories(quantity: Int) = Seq(calories * quantity) ++ toStats(quantity)
  }

  val pattern = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  val input = Source.fromURL(getClass.getResource("day15.txt")).getLines()

  val ingredients = SortedMap(input.map {
    case pattern(name, a, b, c, d, e) => name -> Ingredient(a.toInt, b.toInt, c.toInt, d.toInt, e.toInt)
  }.toList: _*)

  val desiredTeaspoons = 100

  val combinations = List.fill(ingredients.size)(0 to desiredTeaspoons)
    .flatten
    .combinations(ingredients.size)
    .filter(_.sum == desiredTeaspoons)
    .flatMap(_.permutations)

  val scores = combinations.map(_.zip(ingredients.values)).toSeq

  def calculateScore(ingredients: List[(Int, Ingredient)]): Int = ingredients
    .map { case (quantity, ingredient) => ingredient.toStats(quantity) }
    .transpose
    .map(_.sum.max(0))
    .product

  val part1Answer = scores.map(calculateScore).max
  println(s"Part 1: ${part1Answer}")

  def calculateScoresWithCalories(ingredients: List[(Int, Ingredient)]): List[Int] = ingredients
    .map { case (quantity, ingredient) => ingredient.toStatsWithCalories(quantity) }
    .transpose
    .map(_.sum.max(0))

  val desiredCalories = 500

  val part2Answer = scores
    .map(calculateScoresWithCalories)
    .filter(_.head == desiredCalories)
    .map(_.tail.product)
    .max
  println(s"Part 2: ${part2Answer}")
}
