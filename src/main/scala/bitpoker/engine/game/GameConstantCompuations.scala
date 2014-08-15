package bitpoker.engine.game
//Collection of utility game computations

import bitpoker.engine.cards.Card
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._
import bitpoker.engine.game.denominations.Denomination

object GameConstantComputations {
  def getNumSeats: GameComputation[Int] =
    get(_.settings.numSeats)

  def getMinBuyIn: GameComputation[Int] =
    get(_.settings.minBuyIn)

  def getMaxBuyIn: GameComputation[Int] =
    get(_.settings.maxBuyIn)

  def getSmallBlind: GameComputation[Int] =
    get(_.settings.smallBlind)

  def getBigBlind: GameComputation[Int] =
    get(_.settings.bigBlind)

  def getDenomination: GameComputation[Denomination] =
    get(_.settings.denomination)
}