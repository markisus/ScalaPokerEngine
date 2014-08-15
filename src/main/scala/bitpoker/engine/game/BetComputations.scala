package bitpoker.engine.game

import UtilityComputations._
import GameComputation._
import CallComputations._
import Utility._
import MoveComputations._

object BetComputations {

  def betOrAllIn(
    userid: Int,
    amount: Int): GameComputation[Boolean] = {
    for {
      maybeUserMoney <- getUserMoney(userid)
      maybeBetOrAllIn = maybeUserMoney.map[GameComputation[Boolean]](userMoney => {
        val amountToBet = amount min userMoney
        val goingAllIn = amount >= userMoney

        for {
          betBefore <- getCurrentBet
          newRoundBet <- incrementRoundBet(userid, amountToBet)
          _ <- incrementPot(amountToBet)
          _ <- incrementTotalBet(userid, amountToBet)
          betAfter <- getCurrentBet
          _ <- setUserMoney(userid, userMoney - amountToBet)
          _ <- if (goingAllIn) setUserState(userid, AllIn)
          else setUserState(userid, Ready)
          amountRaised = betAfter - betBefore
          minRaise <- getMinRaise
          _ <- setMinRaise(amountRaised max minRaise)
        } yield true
      })
      result <- maybeBetOrAllIn.getOrElse(emit(false)) //Report failure
    } yield result
  }

  def betPossibility(
    userid: Int): GameComputation[Either[ActionImpossibility, CanBet]] = {
    possibleIfUserActive(userid,
      for {
        minRaise <- getMinRaise
        amountNeededToCall <- getAmountNeededToCall(userid)
        minBetAmount = amountNeededToCall + minRaise
        userMoney <- getUserMoney(userid)
        name <- getName(userid)

        hasEnoughMoney = userMoney.map(_ >= minBetAmount).getOrElse(false)
        result <- {
          if (!hasEnoughMoney)
            emit(NotEnoughMoney())
              .map(Left(_))
          else
            emit(CanBet(
              minBetAmount,
              userMoney.getOrElse[Int](minBetAmount)))
              .map(Right(_))
        }
      } yield {
        result
      })
  }

  def bet(
    userid: Int,
    betAmount: Int): GameComputation[Either[ActionImpossibility, PlayerBet]] = {

    val betAction: CanBet => GameComputation[Either[ActionImpossibility, PlayerBet]] =
      canBet =>
        for {
          result <- if (isInRange(betAmount, canBet.minBet, canBet.maxBet))
            betOrAllIn(userid, betAmount)
              .andThen(
                emit(PlayerBet(userid, betAmount)).map(Right(_)))
          else
            emit(BetOutOfRange()).map(Left(_))
        } yield result

    lazy val possiblity = betPossibility(userid)

    makeMoveComputation(
      userid,
      possiblity,
      betAction)
  }

}