/**
  * Created by Jian Yuan on 12/23/2015.
  */
object Day21 extends App {
  case class ItemStats(cost: Int = 0, damage: Int = 0, armor: Int = 0)

  case class PlayerStats(hp: Int = 0, damage: Int = 0, armor: Int = 0, cost: Int = 0) {
    def attackedBy(other: PlayerStats): Option[PlayerStats] = {
      val damage = (other.damage - armor).max(1)
      if (hp <= damage) None
      else Some(copy(hp = hp - damage))
    }

    def apply(item: ItemStats): PlayerStats = copy(
      damage = damage + item.damage,
      armor = armor + item.armor,
      cost = cost + item.cost
    )
  }

  def playerWins(attacker: PlayerStats, defender: PlayerStats): Boolean = defender.attackedBy(attacker) match {
    case None => true
    case Some(attackedDefender) => !playerWins(attackedDefender, attacker)
  }

  /*
    Weapons:    Cost  Damage  Armor
    Dagger        8     4       0
    Shortsword   10     5       0
    Warhammer    25     6       0
    Longsword    40     7       0
    Greataxe     74     8       0

    Armor:      Cost  Damage  Armor
    Leather      13     0       1
    Chainmail    31     0       2
    Splintmail   53     0       3
    Bandedmail   75     0       4
    Platemail   102     0       5

    Rings:      Cost  Damage  Armor
    Damage +1    25     1       0
    Damage +2    50     2       0
    Damage +3   100     3       0
    Defense +1   20     0       1
    Defense +2   40     0       2
    Defense +3   80     0       3
   */

  val weapons: Set[ItemStats] = Set(
    ItemStats(8, 4, 0),
    ItemStats(10, 5, 0),
    ItemStats(25, 6, 0),
    ItemStats(40, 7, 0),
    ItemStats(74, 8, 0)
  )
  val armors: Set[ItemStats] = Set(
    ItemStats(), // No armor
    ItemStats(13, 0, 1),
    ItemStats(31, 0, 2),
    ItemStats(53, 0, 3),
    ItemStats(75, 0, 4),
    ItemStats(102, 0, 5)
  )
  val rings: Set[ItemStats] = Set(
    ItemStats(), // No ring 1
    ItemStats(), // No ring 2
    ItemStats(25, 1, 0),
    ItemStats(50, 2, 0),
    ItemStats(100, 3, 0),
    ItemStats(20, 0, 1),
    ItemStats(40, 0, 2),
    ItemStats(80, 0, 3)
  )

  val player = PlayerStats(hp = 100)
  val boss = PlayerStats(hp = 104, damage = 8, armor = 1)

  val itemCombos = for {
    weapon <- weapons
    armor <- armors
    ring1 <- rings
    ring2 <- rings - ring1
  } yield player(weapon)(armor)(ring1)(ring2)

  val winningItemCombos = itemCombos.filter(playerWins(_, boss))
  val part1Answer = winningItemCombos.map(_.cost).min
  println(s"Part 1: $part1Answer")

  val losingItemCombos = itemCombos.filterNot(playerWins(_, boss))
  val part2Answer = losingItemCombos.map(_.cost).max
  println(s"Part 2: $part2Answer")
}
