import java.security.MessageDigest

import scala.io.Source

/**
 * Created by jianyuan on 06/12/15.
 */
object Day4 extends App {
  val input = Source.fromURL(getClass.getResource("day4.txt")).getLines().mkString

  def md5(in: String): String = MessageDigest.getInstance("MD5").digest(in.getBytes).map("%02x".format(_)).mkString

  val iterator = Iterator.from(1)

  def findPrefix[T](iter: Iterator[T], prefix: String): T =
    iter.find(candidate => md5(input + candidate.toString).startsWith(prefix)).get

  println(s"Answer (5 zeros): ${findPrefix(iterator, "00000")}")
  println(s"Answer (6 zeros): ${findPrefix(iterator, "000000")}")
}
