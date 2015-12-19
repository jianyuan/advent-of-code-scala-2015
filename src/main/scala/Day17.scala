import scala.io.Source

/**
  * Created by Jian Yuan on 19/12/15.
  */
object Day17 extends App {
  val input = Source.fromURL(getClass.getResource("day17.txt")).getLines()

  val containers = input.map(_.toInt).toList.sorted

  val combinations = (1 to containers.size)
    .flatMap(containers.indices.combinations)
    .map(_.map(containers.apply))

  val targetSize = 150

  val targetCombinations = combinations.filter(_.sum == targetSize)

  val part1Answer = targetCombinations.size
  println(s"Part 1: $part1Answer")

  val targetCombinationSizes = targetCombinations.map(_.size)
  val minimumContainers = targetCombinationSizes.min
  val part2Answer = targetCombinationSizes.count(_ == minimumContainers)
  println(s"Part 1: $part2Answer")
}
