import scala.annotation.tailrec

/**
 * Created by Jian Yuan on 11/12/2015.
 */
object Day11 extends App {

  val alphabets = "abcdefghjkmnpqrstuvwxyz"
  val numAlphabets = alphabets.length

  def decode(input: Seq[Char]): Seq[Int] = input.toList.map(alphabets.indexOf(_))

  def encode(input: Seq[Int]): String = input.map(alphabets(_)).mkString

  def increment(input: Seq[Int]): Seq[Int] = input.scanRight((0, 1)) {
    case (digit, (_, carry)) =>
      val next = digit + carry
      (next % numAlphabets, next / numAlphabets)
  }.dropRight(1).map(_._1)

  @tailrec
  def hasTwoPairs(remaining: Seq[Int], lastPair: Option[Int] = None): Boolean = remaining match {
    case Nil => false
    case x :: y :: tail if x == y => lastPair match {
      case Some(a) if a != x => true
      case _ => hasTwoPairs(tail, Some(x))
    }
    case _ :: tail => hasTwoPairs(tail, lastPair)
  }

  def hasOneIncreasingStraightOfAtLeastThreeLetters(input: Seq[Int]): Boolean = input.sliding(3).exists {
    case Seq(a, b, c) => b - a == 1 && c - b == 1
  }

  def nextPasswords(input: Seq[Int]): Stream[Seq[Int]] = {
    lazy val passwords: Stream[Seq[Int]] = input #:: increment(input) #:: passwords
      .zip(passwords.tail)
      .map(password => increment(password._2))
    passwords
  }

  def nextPasswordsWithRules(input: Seq[Int]): Stream[Seq[Int]] = nextPasswords(input)
    .filter(hasOneIncreasingStraightOfAtLeastThreeLetters)
    .filter(hasTwoPairs(_))

  val rawInput = "hxbxwxba"
  val input = decode(rawInput)
  println("Next passwords are:")
  nextPasswordsWithRules(input) take 2 map encode foreach println

}
