import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by Jian Yuan on 12/12/2015.
 */
object Day12 extends App {

  sealed trait Expr

  case class Obj(members: List[Expr]) extends Expr

  case class Pair(key: Expr, value: Expr) extends Expr

  case class Array(elements: List[Expr]) extends Expr

  case class Str(value: String) extends Expr

  case class Integer(value: Int) extends Expr

  object JSONParser extends JavaTokenParsers {
    def obj: Parser[Expr] = '{' ~> repsep(pair, ",") <~ '}' ^^ Obj

    def pair: Parser[Expr] = string ~ ':' ~ value ^^ {
      case string ~ _ ~ value => Pair(string, value)
    }

    def array: Parser[Expr] = '[' ~> repsep(value, ",") <~ ']' ^^ Array

    def string: Parser[Expr] = '"' ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+""".r <~ '"' ^^ Str

    def number: Parser[Expr] = integer

    def integer: Parser[Expr] = wholeNumber ^^ { value => Integer(value.toInt) }

    def value: Parser[Expr] = string | number | obj | array

    def apply(input: String): Expr = parseAll(value, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  def containsPropertyValueRed(input: Expr): Boolean = input match {
    case Obj(members) => members.exists(containsPropertyValueRed)
    case Pair(_, Str("red")) => true
    case _ => false
  }

  def sumNumbers(input: Expr): Int = input match {
    case Obj(members) => members.map(sumNumbers).sum
    case Pair(_, member) => sumNumbers(member)
    case Array(elements) => elements.map(sumNumbers).sum
    case Integer(value) => value
    case _ => 0
  }

  def sumNumbersExcludingObjectWithPropertyValueRed(input: Expr): Int = input match {
    case obj: Obj if containsPropertyValueRed(obj) => 0
    case Obj(members) => members.map(sumNumbersExcludingObjectWithPropertyValueRed).sum
    case Pair(_, member) => sumNumbersExcludingObjectWithPropertyValueRed(member)
    case Array(elements) => elements.map(sumNumbersExcludingObjectWithPropertyValueRed).sum
    case Integer(value) => value
    case _ => 0
  }

  val rawJSON = Source.fromURL(getClass.getResource("day12.json")).getLines().mkString
  val jsonExpr = JSONParser(rawJSON)
  println(s"Part 1: ${sumNumbers(jsonExpr)}")
  println(s"Part 2: ${sumNumbersExcludingObjectWithPropertyValueRed(jsonExpr)}")

}
