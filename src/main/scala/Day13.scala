import scala.io.Source

/**
  * Created by Jian Yuan on 13/12/15.
  */
object Day13 extends App {

  type Person = String
  type HappinessUnit = Int
  type SeatingHappinessUnitMap = Map[(Person, Person), HappinessUnit]

  val inputRegex = """(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\.""".r

  def parseHappinessUnits(rawInput: Seq[String]): SeatingHappinessUnitMap = rawInput.map {
    case inputRegex(from, gainLose, value, to) => gainLose match {
      case "gain" => (from, to) -> value.toInt
      case "lose" => (from, to) -> -value.toInt
    }
  }.toMap

  def uniqueNames(units: SeatingHappinessUnitMap): List[Person] =
    units.keySet.flatMap(key => Set(key._1, key._2)).toList

  def seatingPermutations(names: List[Person]): List[List[Person]] = names.permutations.map {
    case x :: xs => x :: xs ::: List(x)
    case Nil => throw new RuntimeException
  }.toList

  def calculateMaximumHappinessUnits(units: SeatingHappinessUnitMap, seatingPermutations: List[List[Person]]): Int =
    seatingPermutations.map(_.sliding(2).foldLeft(0) {
      case (sum, List(a, b)) => sum + units.getOrElse((a, b), 0) + units.getOrElse((b, a), 0)
    }).max

  val rawInput = Source.fromURL(getClass.getResource("day13.txt")).getLines().toSeq
  val happinessUnits = parseHappinessUnits(rawInput)

  val names = uniqueNames(happinessUnits)

  val answer = calculateMaximumHappinessUnits(happinessUnits, seatingPermutations(names))
  println(s"Maximum happiness units: $answer")

  val answer2 = calculateMaximumHappinessUnits(happinessUnits, seatingPermutations("Jian" :: names))
  println(s"Maximum happiness units (including me): $answer2")

}
