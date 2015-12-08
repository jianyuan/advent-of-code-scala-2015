import scala.annotation.tailrec
import scala.io.Source

/**
 * Created by Jian Yuan on 08/12/2015.
 */
object Day8 extends App {

  def decodedStringLength(input: String): Int = {
    @tailrec
    def decodeCount(chars: List[Char], length: Int = 0): Int = chars match {
      case Nil => length
      case '\\' :: 'x' :: a :: b :: tail => decodeCount(tail, length + 1)
      case '\\' :: '"' :: tail => decodeCount(tail, length + 1)
      case '\\' :: '\\' :: tail => decodeCount(tail, length + 1)
      case '"' :: tail => decodeCount(tail, length)
      case _ :: tail => decodeCount(tail, length + 1)
    }

    decodeCount(input.toList)
  }

  def encodeString(input: String): String = {
    @tailrec
    def encode(chars: List[Char], res: List[Char] = List.empty): List[Char] = chars match {
      case '"' :: tail => encode(tail, '\\' :: '"' :: res)
      case '\\' :: tail => encode(tail, '\\' :: '\\' :: res)
      case a :: tail => encode(tail, a :: res)
      case _ => res
    }
    ('\"' :: encode(input.toList).mkString :: '\"' :: Nil).mkString
  }

  val strings = Source.fromURL(getClass.getResource("day8.txt")).getLines().toList

  val literalCount = strings.map(_.length).sum

  // Part 1
  val memoryCount = strings.map(decodedStringLength).sum
  println(s"Answer (Part 1): ${literalCount - memoryCount}")

  // Part 2
  val memoryCount2 = strings.map(encodeString).map(_.length).sum
  println(s"Answer (Part 2): ${memoryCount2 - literalCount}")
}
