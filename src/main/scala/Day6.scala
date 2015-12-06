import scala.io.Source

/**
 * Created by Jian Yuan on 12/6/2015.
 */
object Day6 extends App {

  case class Coordinate(x: Int, y: Int)

  abstract class Instruction(from: Coordinate, to: Coordinate, mutator: Boolean => Boolean) {
    def apply(state: Array[Array[Boolean]]): Unit = {
      for {
        x <- from.x to to.x
        y <- from.y to to.y
      } state(x)(y) = mutator(state(x)(y))
    }
  }

  object Instruction {
    val regex = """(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)""".r

    def unapply(in: String): Option[Instruction] = in match {
      case Instruction.regex(instruction, x1, y1, x2, y2) =>
        val from = Coordinate(x1.toInt, y1.toInt)
        val to = Coordinate(x2.toInt, y2.toInt)
        instruction match {
          case "turn on" => Some(TurnOn(from, to))
          case "toggle" => Some(Toggle(from, to))
          case "turn off" => Some(TurnOff(from, to))
          case _ => None
        }
      case _ => None
    }
  }

  case class TurnOn(from: Coordinate, to: Coordinate) extends Instruction(from, to, _ => true)

  case class Toggle(from: Coordinate, to: Coordinate) extends Instruction(from, to, state => !state)

  case class TurnOff(from: Coordinate, to: Coordinate) extends Instruction(from, to, _ => false)

  val input = Source.fromURL(getClass.getResource("day6.txt")).getLines().toSeq

  val instructions: Seq[Instruction] = input.flatMap(Instruction.unapply)

  val state = Array.ofDim[Boolean](1000, 1000)
  instructions.foreach(_ (state))

  val lightsLit = state.map(_.count(identity)).sum
  println(s"Number of lights lit: $lightsLit")

}
