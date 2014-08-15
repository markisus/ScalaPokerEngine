package bitpoker.engine.game

import bitpoker.engine.game.GameConstantComputations._
import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._
import BetComputations._
import MoveComputations._

object CallComputations {
  def callPossibility(
    userid: Int): GameComputation[Either[ActionImpossibility, CanCall]] = {
    possibleIfUserActive(userid,
      emit(CanCall()).map(Right(_)))
  }

  def call(
    userid: Int): GameComputation[Either[ActionImpossibility, PlayerCalled]] = {
    val callAction: GameComputation[Either[ActionImpossibility, PlayerCalled]] =
      for {
        amountNeededToCall <- getAmountNeededToCall(userid)
        currentBet <- getCurrentBet
        _ <- betOrAllIn(userid, amountNeededToCall)
        _ <- addToUsersWhoHadTurn(userid)
        result <- emit(PlayerCalled(userid, amountNeededToCall)).map(Right(_))
      } yield result

    makeMoveComputation(
      userid,
      callPossibility(userid),
      (_: CanCall) => callAction)
  }

  def getAmountNeededToCall(userid: Int) =
    for {
      currentBet <- getCurrentBet
      userRoundBet <- getRoundBet(userid)
      amountNeededToCall = userRoundBet.map(currentBet - _).getOrElse(0)
    } yield amountNeededToCall
}