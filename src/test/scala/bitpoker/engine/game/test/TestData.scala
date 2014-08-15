package bitpoker.engine.game.test

import bitpoker.engine.game.GameSettings
import bitpoker.engine.game.GameKernel

object TestGameKernel {
  val gameSettings = new GameSettings(
    numSeats = 10,
    minBuyIn = 1,
    maxBuyIn = Int.MaxValue / 2,
    smallBlind = 1,
    bigBlind = 2)
  
  val gameKernel = new GameKernel(gameSettings)
}