import scala.io.Source

/**
 * Created by jianyuan on 06/12/15.
 */
object Day5 extends App {
  val input = Source.fromURL(getClass.getResource("day5.txt")).getLines().toSeq

  def matcher(pattern: String): String => Boolean = pattern.r.unanchored.findFirstIn(_).isDefined

  // Part 1
  val threeVowels: String => Boolean = matcher( """[aeiou].*[aeiou].*[aeiou]""")
  val twiceInARow: String => Boolean = matcher( """(\w)\1""")
  val noNaughtyStrings: String => Boolean = matcher( """(ab|cd|pq|xy)""").andThen(!_)

  def isNice(in: String): Boolean = threeVowels(in) && twiceInARow(in) && noNaughtyStrings(in)

  println(s"There are ${input.count(isNice)} nice string(s)")

  // Part 2
  val pairAtLeastTwice: String => Boolean = matcher( """(\w)(\w).*\1\2""")
  val sandwiched: String => Boolean = matcher( """(\w)\w\1""")

  def isNice2(in: String): Boolean = pairAtLeastTwice(in) && sandwiched(in)

  println(s"There are ${input.count(isNice2)} nice string(s) under new rules")

}
