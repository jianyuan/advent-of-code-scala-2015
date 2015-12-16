import scala.io.Source

/**
  * Created by Jian Yuan on 15/12/15.
  */
object Day14 extends App {
  val pattern = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  case class Stats(speed: Int, flyTime: Int, restTime: Int) {
    lazy val totalTime = flyTime + restTime
  }

  def finalDistanceCalculator(duration: Int)(stats: Stats): Int = {
    val distance = (duration / stats.totalTime) * stats.flyTime
    val remainingDistance = math.min(duration % stats.totalTime, stats.flyTime)
    (distance + remainingDistance) * stats.speed
  }

  val rawInput = Source.fromURL(getClass.getResource("day14.txt")).getLines().toSeq

  val reindeer = rawInput.map {
    case pattern(name, speed, flyTime, restTime) => name -> Stats(speed.toInt, flyTime.toInt, restTime.toInt)
  }.toMap

  val distance = 2503

  val distanceTraveled = reindeer.mapValues(finalDistanceCalculator(distance)).maxBy(_._2)
  println(s"Distance traveled: $distanceTraveled")

  val raceTimeline = (1 to distance).map(second => reindeer.mapValues(finalDistanceCalculator(second)))
  val winningPoints = raceTimeline
    .flatMap { distances =>
      val maxDistance = distances.maxBy(_._2)._2
      distances.filter(_._2 == maxDistance).keys
    }
    .groupBy(identity)
    .mapValues(_.length)
    .maxBy(_._2)

  println(s"Points $winningPoints")
}
