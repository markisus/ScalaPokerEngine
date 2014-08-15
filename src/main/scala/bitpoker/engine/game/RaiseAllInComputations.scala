package bitpoker.engine.game

import UtilityComputations._
import GameComputation._
import CallComputations._
import BetComputations._
import Utility._
import MoveComputations._

object RaiseAllInComputations {

  def raiseAllInPossibility(
    userid: Int): GameComputation[Either[ActionImpossibility, CanRaiseAllIn]] = {
    possibleIfUserActive(userid,
      for {
        minRaise <- getMinRaise
        amountNeededToCall <- getAmountNeededToCall(userid)
        userMoney <- getUserMoney(userid)
        hasEnoughMoney = userMoney.map(_ > amountNeededToCall).getOrElse(false)
        result <- {
          if (!hasEnoughMoney)
            emit(Left(NotEnoughMoney()))
          else
            emit(Right(CanRaiseAllIn()))
        }
      } yield result)
  }

  def raiseAllIn(
    userid: Int): GameComputation[Either[ActionImpossibility, PlayerRaisedAllIn]] = {

    val raiseAllInAction: CanRaiseAllIn => GameComputation[Either[ActionImpossibility, PlayerRaisedAllIn]] =
      canRaiseAllIn =>
        for {
          userMoney <- getUserMoney(userid)
          _ <- userMoney.map(money => betOrAllIn(userid, money)).getOrElse(noop)
        } yield Right(PlayerRaisedAllIn(userid))

    makeMoveComputation(
      userid,
      raiseAllInPossibility(userid),
      raiseAllInAction)
  }
}