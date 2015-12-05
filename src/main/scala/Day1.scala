import scala.io.Source

/**
 * Created by jianyuan on 05/12/15.
 */
object Day1 extends App {
  val input = Source.fromURL(getClass.getResource("day1.txt")).getLines().mkString

  val steps = input.map {
    case '(' => 1
    case ')' => -1
  }

  val positions = steps.scanLeft(0)(_ + _)

  println(s"Final floor: ${positions.last}")
  println(s"First enter basement at: ${positions.indexOf(-1)}")
}
