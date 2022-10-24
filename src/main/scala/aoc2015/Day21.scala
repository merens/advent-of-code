package aoc2015

import scala.annotation.tailrec

object Day21 {

  case class Player(hp: Int, dmg: Int, armor: Int) {
    def +(weapon: Weapon): Player = Player(hp, dmg + weapon.dmg, armor + weapon.armor)
    def +(armor: Armor): Player   = Player(hp, dmg + armor.dmg, this.armor + armor.armor)
    def +(ring: Ring): Player     = Player(hp, dmg + ring.dmg, armor + ring.armor)
  }
  case class Weapon(cost: Int, dmg: Int, armor: Int)
  case class Armor(cost: Int, dmg: Int, armor: Int)
  // TODO: all shop classes are similar, trait or only one class?
  case class Ring(cost: Int, dmg: Int, armor: Int) {
    def +(that: Ring): Ring = Ring(cost + that.cost, dmg + that.dmg, armor + that.armor)
  }

  // TODO: parse?
  val weapons: Seq[Weapon] = Seq(
    Weapon(8, 4, 0),
    Weapon(10, 5, 0),
    Weapon(25, 6, 0),
    Weapon(40, 7, 0),
    Weapon(74, 8, 0)
  )

  val armors: Seq[Armor] = Seq(
    Armor(13, 0, 1),
    Armor(31, 0, 2),
    Armor(53, 0, 3),
    Armor(75, 0, 4),
    Armor(102, 0, 5)
  )

  val rings: Seq[Ring] = Seq(
    Ring(25, 1, 0),
    Ring(50, 2, 0),
    Ring(100, 3, 0),
    Ring(20, 0, 1),
    Ring(40, 0, 2),
    Ring(80, 0, 3)
  )

  def parseInput(str: String): Seq[Player] =
    for {
      hp    <- "Hit Points: (\\d+)".r.findFirstMatchIn(str).get.group(1)
      dmg   <- "Damage: (\\d+)".r.findFirstMatchIn(str).get.group(1)
      armor <- "Armor: (\\d+)".r.findFirstMatchIn(str).get.group(1)
    } yield Player(hp, dmg, armor)

//  2 parts: fight logic + brute force shop stats
  @tailrec
  def isPlayerWinningFight(p: Player, boss: Player, isPlayerTurn: Boolean): Boolean =
//    println(s"p: $p boss: $boss")
    if (isPlayerTurn) {
      val bossHp = boss.hp - Math.max(1, p.dmg - boss.armor)
      if (bossHp <= 0) true
      else isPlayerWinningFight(p, boss.copy(hp = bossHp), isPlayerTurn = false)
    } else {
      val playerHp = p.hp - Math.max(1, boss.dmg - p.armor)
      if (playerHp <= 0) false
      else isPlayerWinningFight(p.copy(hp = playerHp), boss, isPlayerTurn = true)
    }

  def pickShopConfiguration(
    weapons: Seq[Weapon],
    armors: Seq[Armor],
    rings: Seq[Ring]
  ): Seq[(Weapon, Armor, Ring)] = {
    val ringPairs         = rings.permutations.map(_.take(2).reduce(_ + _)).distinct.toSeq
    val ringCombinations  = ringPairs appendedAll rings :+ Ring(0, 0, 0)
    val armorCombinations = armors :+ Armor(0, 0, 0)
    weapons.flatMap(w => armorCombinations.flatMap(a => ringCombinations.map(r => (w, a, r))))
  }

  def fightWithShop(configs: Seq[(Weapon, Armor, Ring)], player: Player, boss: Player): Int =
    configs
      .filter(config => isPlayerWinningFight(player + config._1 + config._2 + config._3, boss, isPlayerTurn = true))
      .map(config => config._1.cost + config._2.cost + config._3.cost)
      .min

  def fightAgainstShop(configs: Seq[(Weapon, Armor, Ring)], player: Player, boss: Player): Int =
    configs
      .filter(config => !isPlayerWinningFight(player + config._1 + config._2 + config._3, boss, isPlayerTurn = true))
      .map(config => config._1.cost + config._2.cost + config._3.cost)
      .max

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val boss   = Player(100, 8, 2)
    val player = Player(100, 0, 0)

    val shop = pickShopConfiguration(weapons, armors, rings)
    println(fightWithShop(shop, player, boss))
    println(fightAgainstShop(shop, player, boss))

  }
}
