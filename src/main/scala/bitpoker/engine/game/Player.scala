package bitpoker.engine.game

import bitpoker.engine.cards.Card
import bitpoker.engine.game.typedefs.Userid

case class PlayerFacade(
  userid: Userid,
  money: Long,
  state: PlayerState,
  roundBet: Long,
  name: String)

class Player(
    val userid: Userid,
    startingMoney: Long,
    startingState: PlayerState,
    val name: String = "Anonymous") {

  private var _money: Long = 0
  private var _roundBet: Long = 0
  private var _totalBet: Long = 0
  private var _holeCards: Seq[Card] = Seq.empty
  var state: PlayerState = JustJoined
  var hadTurn: Boolean = false

  money = startingMoney
  state = startingState

  def money = _money
  def money_=(value: Long) = {
    require(value >= 0, "money must be positive")
    _money = value
  }

  def roundBet = _roundBet
  def roundBet_=(value: Long) = {
    require(value >= 0, "roundbet must be positive")
    _roundBet = value
  }

  def totalBet = _totalBet
  def totalBet_=(value: Long) = {
    require(value >= 0, "totalbet must be positive")
    _totalBet = value
  }

  def holeCards = _holeCards
  def holeCards_=(value: Seq[Card]) = {
    require(value.size == 0 || value.size == 2, "num holecards must be 0 or 2")
    _holeCards = value
  }

  def makeFacade() = {
    PlayerFacade(userid, money, state, roundBet, name)
  }
}