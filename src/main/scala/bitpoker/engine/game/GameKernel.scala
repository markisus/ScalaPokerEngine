package bitpoker.engine.game

import bitpoker.engine.cards.Card
import bitpoker.engine.game._
import bitpoker.engine.game.denominations._

case class GameKernel(
    settings: GameSettings,
    minRaise: Int = 0,
    dealerPosition: Int = 0,
    activeUserPosition: Int = 0,
    smallBlindPosition: Int = 0,
    bigBlindPosition: Int = 0,
    pot: Int = 0,
    currentBet: Int = 0,
    usersWhoHadTurn: Set[Int] = Set.empty,
    deck: Seq[Card] = Seq.empty,
    gameState: GameState = WaitForPlayers,
    communityCards: Seq[Card] = Seq.empty,
    seatNumbersToUserids: Map[Int, Int] = Map.empty,
    userMaps: UserMaps = UserMaps(),
    results: GameResults = GameResults(),
    version: Int = 0) {

  def isUserSeated(userid: Int): Boolean =
    seatNumbersToUserids
      .values
      .toSet
      .contains(userid)
}

case class GameSettings(
  numSeats: Int,
  minBuyIn: Int,
  maxBuyIn: Int,
  smallBlind: Int,
  bigBlind: Int,
  denomination: Denomination = x1,
  usePlayMoney: Boolean = true)

case class UserMaps(
  useridsToStates: Map[Int, PlayerState] = Map.empty,
  useridsToMoney: Map[Int, Int] = Map.empty,
  useridsToHoleCards: Map[Int, Seq[Card]] = Map.empty,
  useridsToRoundBets: Map[Int, Int] = Map.empty,
  useridsToNames: Map[Int, String] = Map.empty,
  useridsToTotalBets: Map[Int, Int] = Map.empty)
  