package aoc2015

import aoc2015.Day21.{Player, isPlayerWinningFight}
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {
  test("test fight") {
    val player = Player(8, 5, 5)
    val boss = Player(12, 7, 2)
    assert(isPlayerWinningFight(player, boss, isPlayerTurn = true))
  }
}
