import scala.io.Source

/**
  * Created by Jian Yuan on 19/12/15.
  */
object Day16 extends App {
  val input = Source.fromURL(getClass.getResource("day16.txt")).getLines()

  def parseCompounds(input: String): Map[String, Int] = input
    .split(", ")
    .map(_.split(": "))
    .map(parts => parts(0) -> parts(1).toInt)
    .toMap


  val parsed: Map[Int, Map[String, Int]] = input
    .map(_.split(": ", 2))
    .map(parts => parts(0).split(' ')(1).toInt -> parseCompounds(parts(1)))
    .toMap

  val target = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  val part1Answer = parsed.find(!_._2.exists(compound => target(compound._1) != compound._2)).get._1
  println(s"Part 1: Sue $part1Answer")

  val part2Answer = parsed.find(!_._2.exists(compound => compound._1 match {
    case "cats" | "trees" => target(compound._1) >= compound._2
    case "pomeranians" | "goldfish" => target(compound._1) <= compound._2
    case _ => target(compound._1) != compound._2
  })).get._1
  println(s"Part 2: Sue $part2Answer")
}
