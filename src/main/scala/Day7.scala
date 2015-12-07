import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Created by jianyuanlee on 07/12/2015.
 */
object Day7 extends App {

  sealed trait Expr

  case class Set(variable: Variable, expr: Expr) extends Expr

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
  val lshiftRegex = """(\w+) LSHIFT (\d+) -> (\w+)""".r
  val rshiftRegex = """(\w+) RSHIFT (\d+) -> (\w+)""".r
  val notRegex = """NOT (\w+) -> (\w+)""".r

  def constOrVariable(expr: String): Expr = Try(expr.toInt) match {
    case Success(value) => Const(value)
    case Failure(_) => Variable(expr)
  }

  def parseInstructions(instructions: List[String]): List[Expr] = {
    instructions.map {
      case wireRegex(from, to) => Set(Variable(to), constOrVariable(from))
      case andRegex(left, right, to) => Set(Variable(to), And(constOrVariable(left), constOrVariable(right)))
      case orRegex(left, right, to) => Set(Variable(to), Or(constOrVariable(left), constOrVariable(right)))
      case lshiftRegex(from, by, to) => Set(Variable(to), LShift(constOrVariable(from), by.toInt))
      case rshiftRegex(from, by, to) => Set(Variable(to), RShift(constOrVariable(from), by.toInt))
      case notRegex(from, to) => Set(Variable(to), Not(constOrVariable(from)))
    }
  }

  def createExprMap(instructions: List[Expr]): mutable.Map[String, Expr] = {
    val table = mutable.Map.empty[String, Expr]
    instructions.foreach {
      case Set(Variable(id), expr) => table(id) = expr
    }
    table
  }

  val rawInstructions = Source.fromURL(getClass.getResource("day7.txt")).getLines().toList

  val instructions: List[Expr] = parseInstructions(rawInstructions)

  //  rawInstructions zip instructions foreach println

  val vars = createExprMap(instructions)

  val bitMask = 0xffff

  def evaluate(vars: mutable.Map[String, Expr], expr: Expr): Int = expr match {
    case Const(value) => value
    case Variable(name) =>
      val value = evaluate(vars, vars(name))
      vars(name) = Const(value)
      value
    case Not(e) => ~evaluate(vars, e) & bitMask
    case And(l, r) => evaluate(vars, l) & evaluate(vars, r)
    case Or(l, r) => evaluate(vars, l) | evaluate(vars, r)
    case LShift(e, by) => (evaluate(vars, e) << by) & bitMask
    case RShift(e, by) => (evaluate(vars, e) >> by) & bitMask
  }

  // Part 1
  val aValue = evaluate(vars, Variable("a"))
  println(s"a = $aValue}")

  // Part 2
  val vars2 = createExprMap(instructions)
  vars2("b") = Const(aValue)
  val newAValue = evaluate(vars2, Variable("a"))
  println(s"a2 = $newAValue}")
}
