import scala.io.Source

/**
  * Created by Jian Yuan on 19/12/15.
  */
object Day18 extends App {
  type Grid = Map[(Int, Int), Boolean]

  val input = Source.fromURL(getClass.getResource("day18.txt")).getLines()

  val states = input
    .map(_.map {
      case '#' => true
      case '.' => false
    }.toList).toList

  val grid: Grid = states
    .map(_.zipWithIndex).zipWithIndex
    .flatMap(ys => ys._1.map(x => (x._2, ys._2) -> x._1)).toMap

  def countNeighbours(grid: Grid, coordinate: (Int, Int)): Int = (for {
    dx <- -1 to 1
    dy <- -1 to 1
    neighbour = (coordinate._1 + dx, coordinate._2 + dy)
    if coordinate != neighbour
  } yield grid.getOrElse(neighbour, false)).count(identity)

  def countLightsOn(grid: Grid): Int = grid.values.count(identity)

  def nextGrid(grid: Grid, stuckedLights: List[(Int, Int)] = List.empty): Grid = grid.map {
    case (coordinate, _) if stuckedLights.contains(coordinate) => coordinate -> true
    case (coordinate, state) => coordinate -> ((state, countNeighbours(grid, coordinate)) match {
      case (true, 2) | (true, 3) => true
      case (false, 3) => true
      case _ => false
    })
  }

  val finalGrid = (1 to 100).foldLeft(grid)((grid, _) => nextGrid(grid))
  println(s"Part 1: ${countLightsOn(finalGrid)}")

  val corners = (for {
    dx <- 0 to 1
    dy <- 0 to 1
  } yield (dx * (states.head.size - 1), dy * (states.size - 1))).toList

  val finalGrid2 = (1 to 100).foldLeft(grid)((grid, _) => nextGrid(grid, corners))
  println(s"Part 2: ${countLightsOn(finalGrid2)}")
}
