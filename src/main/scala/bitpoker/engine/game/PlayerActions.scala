package bitpoker.engine.game

import bitpoker.engine.game.typedefs._

trait Action
trait ActionPossibility

trait ActionImpossibility
trait ActionSuccess

case class CanJoin(
  emptySeats: Set[SeatNumber],
  minBuyIn: Long,
  maxBuyIn: Long) extends ActionPossibility

case class Join(
    seatNumber: SeatNumber,
    buyInAmount: Long,
    name: String = "Anonymous") extends Action {
  def withName(name: String) =
    copy(name = name)
}

case class CanLeave() extends ActionPossibility
case class Leave() extends Action

case class CanRebuy(minRebuy: Long, maxRebuy: Long) extends ActionPossibility
case class Rebuy(rebuyAmount: Long) extends Action

case class CanForceBigBlind() extends ActionPossibility
case class ForceBigBlind() extends Action

case class CanFold() extends ActionPossibility
case class Fold() extends Action

case class CanCall() extends ActionPossibility
case class Call() extends Action

case class CanBet(minBet: Long, maxBet: Long) extends ActionPossibility
case class Bet(betAmount: Long) extends Action

case class CanRaiseAllIn() extends ActionPossibility
case class RaiseAllIn() extends Action

case class CanStart() extends ActionPossibility
case class Start() extends Action

case class Failed() extends ActionImpossibility
case class NotEnoughMoney() extends ActionImpossibility
case class GameIsFull() extends ActionImpossibility
case class UserAlreadyInGame() extends ActionImpossibility
case class GameAlreadyStarted() extends ActionImpossibility
case class BuyInOutOfRange() extends ActionImpossibility
case class SeatTaken() extends ActionImpossibility
case class BetOutOfRange() extends ActionImpossibility
case class NotEnoughPreparedUsers() extends ActionImpossibility
case class NotInGame() extends ActionImpossibility
case class TooMuchMoneyCannotRebuy() extends ActionImpossibility
case class RebuyOutOfRange() extends ActionImpossibility
case class NotThisUsersTurn() extends ActionImpossibility
case class NotJustJoinedCannotForceBigBlind() extends ActionImpossibility

case class Succeeded() extends ActionSuccess
case class Started(
  startedUsername: String,
  smallBlindUsername: String,
  bigBlindUsernames: Seq[String]) extends ActionSuccess
case class PlayerJoined(
  userid: Userid,
  seatNumber: SeatNumber,
  buyIn: Long,
  name: String) extends ActionSuccess
case class PlayerLeft(userid: Userid) extends ActionSuccess
case class PlayerBet(userid: Userid, betAmount: Long) extends ActionSuccess
case class PlayerCalled(userid: Userid, callAmount: Long) extends ActionSuccess
case class PlayerRaisedAllIn(userid: Userid) extends ActionSuccess
case class PlayerFolded(userid: Userid) extends ActionSuccess
case class PlayerRebought(userid: Userid, rebuyAmount: Long) extends ActionSuccess
case class PlayerForcedBigBlind(userid: Userid) extends ActionSuccess