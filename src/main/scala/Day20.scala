/**
  * Created by Jian Yuan on 12/22/2015.
  */
object Day20 extends App {
  def findMinimumHouse(presents: Seq[(Int, Int)], target: Int): Int = presents
    .groupBy(_._1)
    .toList
    .sortBy(_._1)
    .find(_._2.map(_._2).sum >= target)
    .get._1

  val input = 29000000

  val target = input / 10
  val part1Answer = findMinimumHouse(for {
    present <- 1 to target
    house <- present to target by present
  } yield (house, present), target)
  println(s"Part 1: $part1Answer")

  val target2 = input / 11
  val part2Answer = findMinimumHouse(for {
    present <- 1 to target2
    house <- present to target2 by present take 50
  } yield (house, present), target2)
  println(s"Part 2: $part2Answer")
}
