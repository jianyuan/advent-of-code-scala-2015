import scala.io.Source

/**
  * Created by Jian Yuan on 19/12/15.
  */
object Day19 extends App {
  val input = Source.fromURL(getClass.getResource("day19.txt")).getLines()

  val (rawReplacements, rawStart) = input.span(_ != "")
  val replacements = rawReplacements.map(_.split(" => ", 2)).map(parts => (parts(0), parts(1))).toList
  val start: String = rawStart.find(_ != "").get

  def enumerateReplacements(from: String, to: String, input: String): Iterator[String] = input
    .sliding(from.length)
    .zipWithIndex
    .collect {
      case (`from`, i) => input.take(i) + to + input.drop(i + from.length)
    }

  def applyReplacements(replacements: List[(String, String)], input: String): List[String] = replacements
    .flatMap(replacement => enumerateReplacements(replacement._1, replacement._2, input))
    .distinct

  val part1Answer = applyReplacements(replacements, start).size
  println(s"Part 1: $part1Answer")

  val swappedReplacements = replacements.map(_.swap)
  val generations: Stream[String] = start #:: generations.map(applyReplacements(swappedReplacements, _).minBy(_.length))
  val part2Answer = generations.takeWhile(!_.contains("e")).size
  println(s"Part 2: $part2Answer")
}
