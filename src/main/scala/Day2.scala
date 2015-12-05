import scala.io.Source

/**
 * Created by jianyuan on 05/12/15.
 */
object Day2 extends App {
  val lines = Source.fromURL(getClass.getResource("day2.txt")).getLines()

  case class Box(length: Int, width: Int, height: Int) {
    val surfaceAreas: List[Int] = List(length * width, width * height, height * length)
    val smallestArea: Int = surfaceAreas.min
    val surfaceArea: Int = 2 * surfaceAreas.sum + smallestArea
    val perimeterLengths: List[Int] = List(length + width, width + height, height + length).map(_ * 2)
    val volume: Int = length * width * height
    val ribbonLength: Int = perimeterLengths.min + volume
  }

  val boxes = (for {
    line <- lines
    dimensions = line.split("x", 3).map(_.toInt)
    box = dimensions match {
      case Array(l, w, h) => Box(l, w, h)
    }
  } yield box).toList

  val wrappingPaperNeeded = boxes.map(_.surfaceArea).sum
  println(s"Square feet of wrapping paper needed: $wrappingPaperNeeded")

  val ribbonNeeded = boxes.map(_.ribbonLength).sum
  println(s"Feet of ribbon needed: $ribbonNeeded")
}
