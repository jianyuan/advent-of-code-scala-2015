import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Created by Jian Yuan on 07/12/2015.
 */
object Day7 extends App {

  sealed trait Expr

  case class Variable(name: String) extends Expr

  case class Const(value: Int) extends Expr

  case class Not(expr: Expr) extends Expr

  case class And(left: Expr, right: Expr) extends Expr

  case class Or(left: Expr, right: Expr) extends Expr

  case class LShift(expr: Expr, by: Int) extends Expr

  case class RShift(expr: Expr, by: Int) extends Expr

  val wireRegex = """(\w+) -> (\w+)""".r
  val andRegex = """(\w+) AND (\w+) -> (\w+)""".r
  val orRegex = """(\w+) OR (\w+) -> (\w+)""".r
  val lShiftRegex = """(\w+) LSHIFT (\d+) -> (\w+)""".r
  val rShiftRegex = """(\w+) RSHIFT (\d+) -> (\w+)""".r
  val notRegex = """NOT (\w+) -> (\w+)""".r

  def constOrVariable(rawExpr: String): Expr = Try(rawExpr.toInt) match {
    case Success(value) => Const(value)
    case Failure(_) => Variable(rawExpr)
  }

  def parseInstructions(rawInstructions: List[String]): Map[String, Expr] = rawInstructions.map {
    case wireRegex(from, to) => to -> constOrVariable(from)
    case andRegex(left, right, to) => to -> And(constOrVariable(left), constOrVariable(right))
    case orRegex(left, right, to) => to -> Or(constOrVariable(left), constOrVariable(right))
    case lShiftRegex(from, by, to) => to -> LShift(constOrVariable(from), by.toInt)
    case rShiftRegex(from, by, to) => to -> RShift(constOrVariable(from), by.toInt)
    case notRegex(from, to) => to -> Not(constOrVariable(from))
  }.toMap

  def evaluate(wires: Map[String, Expr], expr: Expr): Int = {
    val mutWires = scala.collection.mutable.Map() ++ wires

    def eval(expr: Expr): Int = expr match {
      case Const(value) => value
      case Variable(name) =>
        val value = eval(mutWires(name))
        mutWires(name) = Const(value)
        value
      case Not(e) => ~eval(e) & 0xffff
      case And(l, r) => eval(l) & eval(r)
      case Or(l, r) => eval(l) | eval(r)
      case LShift(e, by) => (eval(e) << by) & 0xffff
      case RShift(e, by) => eval(e) >> by
    }

    eval(expr)
  }

  val rawInstructions = Source.fromURL(getClass.getResource("day7.txt")).getLines().toList

  val instructions: Map[String, Expr] = parseInstructions(rawInstructions)

  // Part 1
  val a = evaluate(instructions, Variable("a"))
  println(s"a (Part 1) = $a")

  // Part 2
  val newInstructions = instructions.updated("b", Const(a))
  val newA = evaluate(newInstructions, Variable("a"))
  println(s"a (Part 2) = $newA")
}
