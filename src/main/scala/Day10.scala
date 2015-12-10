import scala.annotation.tailrec

/**
 * Created by jianyuanlee on 10/12/2015.
 */
object Day10 extends App {

  def processInput(input: String): String = {
    @tailrec
    def process(remaining: List[Char], last: Char, count: Int = 0, result: List[Char] = List.empty): List[Char] = remaining match {
      case Nil => last :: count.toString.toList ::: result
      case head :: tail if head == last => process(tail, head, count + 1, result)
      case head :: tail => process(tail, head, 1, last :: count.toString.toList ::: result)
    }

    input.toList match {
      case Nil => ""
      case head :: tail => process(tail, head, 1, Nil).reverse.mkString
    }
  }

  val fortyTimes = (1 to 40).foldLeft("1321131112")((next, _) => processInput(next)).length
  println(s"40 times: $fortyTimes")
  val fiftyTimes = (1 to 50).foldLeft("1321131112")((next, _) => processInput(next)).length
  println(s"50 times: $fiftyTimes")

}
