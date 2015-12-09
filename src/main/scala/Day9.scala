import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Created by Jian Yuan on 09/12/2015.
 */
object Day9 extends App {

  val distanceRegex = """(\w+) to (\w+) = (\d+)""".r

  val rawDistances = Source.fromURL(getClass.getResource("day9.txt")).getLines().toList

  val distances = rawDistances.map {
    case distanceRegex(from, to, d) => (from -> to) -> d.toInt
  }.toMap

  val places: List[String] = distances.keys.flatMap(p => List(p._1, p._2)).toList.distinct

  val totalDistances = places.permutations.map(_.sliding(2).map {
    case List(a, b) => distances.getOrElse(a -> b, distances(b -> a))
  }.sum).toList

  println(s"Shortest distance: ${totalDistances.min}")
  println(s"Longest distance: ${totalDistances.max}")

}
