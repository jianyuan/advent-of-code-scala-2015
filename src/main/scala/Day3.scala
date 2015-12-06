import scala.annotation.tailrec
import scala.io.Source

/**
 * Created by jianyuan on 05/12/15.
 */
object Day3 extends App {

  case class Coordinate(x: Int, y: Int)

  abstract class Direction(x: Int = 0, y: Int = 0) {
    def apply(in: Coordinate) = Coordinate(in.x + x, in.y + y)
  }

  case object Up extends Direction(y = -1)

  case object Down extends Direction(y = 1)

  case object Left extends Direction(x = -1)

  case object Right extends Direction(x = 1)

  val rawInstructions = Source.fromURL(getClass.getResource("day3.txt")).getLines().mkString

  val instructions: Seq[Direction] = rawInstructions.map {
    case '^' => Up
    case 'v' => Down
    case '<' => Left
    case '>' => Right
  }

  val start = Coordinate(0, 0)

  def moves(instructions: Seq[Direction], start: Coordinate = Coordinate(0, 0)) =
    instructions.scanLeft(start)((coordinate, direction) => direction(coordinate))

  def partition[T](ls: Seq[T]): (Seq[T], Seq[T]) = {
    (ls.grouped(2).map(_.head).toSeq, ls.drop(1).grouped(2).map(_.head).toSeq)
  }

  // Part 1
  println(s"Number of houses receiving at least one present: ${moves(instructions).distinct.length}")

  // Part 2
  val (santaInstructions, roboSantaInstructions) = partition(instructions)
  println(s"With Robo-Santa: ${(moves(santaInstructions) ++ moves(roboSantaInstructions)).distinct.length}")
}
