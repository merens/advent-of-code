package aoc2015

object Day22 {

  case class Player(hp: Int, mana: Int, armor: Int, shieldTime: Int, manaTime: Int)
  case class Boss(hp: Int, dmg: Int, poisonTime: Int)

  case class Spell(index: Int, cost: Int, dmg: Int, heal: Int, shield: Int, poison: Int, manaRegen: Int, duration: Int)

  val spells = Seq(
    Spell(0, 53, 4, 0, 0, 0, 0, 0),
    Spell(1, 73, 2, 2, 0, 0, 0, 0),
    Spell(2, 113, 0, 0, 7, 0, 0, 6),
    Spell(3, 173, 0, 0, 0, 3, 0, 6),
    Spell(4, 229, 0, 0, 0, 0, 101, 5)
  )

  // similar to day 21

  def calculateMana(p: Player, spell: Spell): Player =
    if (p.manaTime > 0)
      p.copy(
        mana = p.mana + spell.manaRegen,
        manaTime = p.manaTime - 1
      )
    else p

  def calculateArmor(p: Player, spell: Spell): Player =
    if (p.shieldTime > 0) p.copy(armor = spell.shield, shieldTime = p.shieldTime - 1)
    else p.copy(armor = 0, shieldTime = 0)

  def calculatePoison(boss: Boss, spell: Spell): Boss =
    if (boss.poisonTime > 0)
      boss.copy(
        hp = boss.hp - spell.poison,
        poisonTime = boss.poisonTime - 1
      )
    else boss

  def calculateSpell(p: Player, boss: Boss, spell: Spell): (Player, Boss) =
    spell.index match {
      case 0 => (p.copy(mana = p.mana - spell.cost), boss.copy(hp = boss.hp - spell.dmg))
      case 1 => (p.copy(hp = p.hp + spell.heal, mana = p.mana - spell.cost), boss.copy(hp = boss.hp - spell.dmg))
      case 2 => (p.copy(mana = p.mana - spell.cost, shieldTime = spell.duration), boss)
      case 3 => (p.copy(mana = p.mana - spell.cost), boss.copy(poisonTime = spell.duration))
      case 4 => (p.copy(mana = p.mana - spell.cost, manaTime = spell.duration), boss)
    }

  def calculateBossDamage(p: Player, boss: Boss): Player =
    p.copy(p.hp - Math.max(1, boss.dmg - p.armor))

  def calculateCost(sequence: Seq[Spell]): Int =
    sequence.map(_.cost).sum

  var min = Integer.MAX_VALUE

  def isPlayerWinningFight(p: Player, boss: Boss, spellSequence: Seq[Spell], isPlayerTurn: Boolean): (Boolean, Int) = {
//    println(s"p: $p boss: $boss spells: ${spellSequence.map(_.index)}")
//    println(s"player health: ${p.hp}, boss health: ${boss.hp}")
    // TODO if a save game state instead of Player and Boss, can I save these reassignments?
    val p1 = calculateMana(p, spells(4))
    val p2 = calculateArmor(p1, spells(2))
    val b1 = calculatePoison(boss, spells(3))
    if (b1.hp <= 0) {
      min = calculateCost(spellSequence)
//      println(min, spellSequence.map(_.index))
      return (true, calculateCost(spellSequence))
    }
    // a way to purge the sequences that cost too much is to use a global var :(
    if (p2.hp <= 0 || calculateCost(spellSequence) > min) return (false, 0)

    if (isPlayerTurn) {
      // is spell x possible?
      var playable = spells.filter(_.cost <= p2.mana)
      if (p2.manaTime > 0) playable = playable diff Seq(spells(4))
      if (b1.poisonTime > 0) playable = playable diff Seq(spells(3))
      if (p2.shieldTime > 0) playable = playable diff Seq(spells(2))
      if (playable.isEmpty) (false, 0)
      else {
        val futures = playable
          .map { spell =>
            val (p3, b2) = calculateSpell(p2, b1, spell)
            isPlayerWinningFight(p3, b2, spellSequence :+ spell, isPlayerTurn = false)
          }
        if (futures.exists(_._1))
          futures
            .filter(_._1)
            .sortBy(_._2)
            .min
        else (false, 0)
      }
    } else isPlayerWinningFight(calculateBossDamage(p2, b1), b1, spellSequence, isPlayerTurn = true)
  }

  def hardMode(p: Player, boss: Boss, spellSequence: Seq[Spell], isPlayerTurn: Boolean): (Boolean, Int) = {
    if (p.hp <= 0 || calculateCost(spellSequence) > min) return (false, 0)
    if (boss.hp <= 0) {
      min = calculateCost(spellSequence)
      println(min, spellSequence.map(_.index))
      return (true, calculateCost(spellSequence))
    }
    val pHard = if (isPlayerTurn) p.copy(hp = p.hp - 1) else p
    val p1    = calculateMana(pHard, spells(4))
    val p2    = calculateArmor(p1, spells(2))
    val b1    = calculatePoison(boss, spells(3))
    if (p2.hp <= 0 || calculateCost(spellSequence) > min) return (false, 0)
    if (b1.hp <= 0) {
      min = calculateCost(spellSequence)
      return (true, calculateCost(spellSequence))
    }

    if (isPlayerTurn) {
      var playable = spells.filter(_.cost <= p2.mana)
      if (p2.manaTime > 0) playable = playable diff Seq(spells(4))
      if (b1.poisonTime > 0) playable = playable diff Seq(spells(3))
      if (p2.shieldTime > 0) playable = playable diff Seq(spells(2))
      if (playable.isEmpty) (false, 0)
      else {
        val futures = playable
          .map { spell =>
            val (p3, b2) = calculateSpell(p2, b1, spell)
            hardMode(p3, b2, spellSequence :+ spell, isPlayerTurn = false)
          }
        if (futures.exists(_._1))
          futures
            .filter(_._1)
            .sortBy(_._2)
            .min
        else (false, 0)
      }
    } else hardMode(calculateBossDamage(p2, b1), b1, spellSequence, isPlayerTurn = true)
  }

  def main(args: Array[String]): Unit = {
    // TODO: parse input
    val boss   = Boss(55, 8, 0)
    val player = Player(50, 500, 0, 0, 0)

    println(isPlayerWinningFight(player, boss, Seq.empty[Spell], isPlayerTurn = true))
    min = Integer.MAX_VALUE
    println(hardMode(player, boss, Seq.empty[Spell], isPlayerTurn = true))
  }
}
