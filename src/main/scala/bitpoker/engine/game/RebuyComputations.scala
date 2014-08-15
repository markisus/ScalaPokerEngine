package bitpoker.engine.game

import bitpoker.engine.game.GameComputation.emit
import bitpoker.engine.game.GameComputation.noop
import bitpoker.engine.game.GameConstantComputations.getMaxBuyIn
import bitpoker.engine.game.GameConstantComputations.getMinBuyIn
import bitpoker.engine.game.Utility.isInRange
import bitpoker.engine.game.UtilityComputations.getGameState
import bitpoker.engine.game.UtilityComputations.getUserMoney
import bitpoker.engine.game.UtilityComputations.getUserMoneyOrDefault
import bitpoker.engine.game.UtilityComputations.getUserState
import bitpoker.engine.game.UtilityComputations.isUserSeated
import bitpoker.engine.game.UtilityComputations.setUserMoney
import bitpoker.engine.game.UtilityComputations.setUserState

object RebuyComputations {
  def rebuyPossibility(
    userid: Int,
    externalMoney: Int = Int.MaxValue): GameComputation[Either[ActionImpossibility, CanRebuy]] =
    for {
      userSeated <- isUserSeated(userid)
      userState <- getUserState(userid)
      isUserInPlay = userState.map(s => Set[PlayerState](AllIn, Ready).contains(s))
        .getOrElse(false)
      gameState <- getGameState
      hasGameStarted = gameState != WaitForPlayers
      minBuyIn <- getMinBuyIn
      maxBuyIn <- getMaxBuyIn
      currentMoney <- getUserMoneyOrDefault(userid)
      minRebuy = 1 max (minBuyIn - currentMoney)
      maxRebuy = maxBuyIn - currentMoney
      result <- {
        if (!userSeated)
          emit(NotInGame()).map(Left(_))
        else if (currentMoney >= maxBuyIn)
          emit(TooMuchMoneyCannotRebuy()).map(Left(_))
        else if (externalMoney < minRebuy)
          emit(NotEnoughMoney()).map(Left(_))
        else if (hasGameStarted && isUserInPlay)
          emit(GameAlreadyStarted()).map(Left(_))
        else
          emit(CanRebuy(minRebuy, maxRebuy)).map(Right(_))
      }
    } yield result

  def rebuy(
    userid: Int,
    rebuyAmount: Int,
    externalMoney: Int): GameComputation[Either[ActionImpossibility, ActionSuccess]] = {
    for {
      possibility <- rebuyPossibility(userid, externalMoney)
      result <- possibility
        .fold[GameComputation[Either[ActionImpossibility, ActionSuccess]]](
          impossibility => emit(impossibility).map(Left(_)),
          canRebuy =>
            if (isInRange(rebuyAmount, canRebuy.minRebuy, canRebuy.maxRebuy))
              rebuyComputation(userid, rebuyAmount)
              .andThen(
                emit(PlayerRebought(userid, rebuyAmount)).map(Right(_)))
            else
              emit(RebuyOutOfRange()).map(Left(_)))
    } yield result
  }

  def rebuyComputation(userid: Int, rebuyAmount: Int) =
    for {
      currentMoney <- getUserMoney(userid)
      _ <- {
        currentMoney.map(currentMoney =>
          setUserMoney(userid, currentMoney + rebuyAmount)
          .andThen(setUserState(userid, Ready)))
          .getOrElse(noop)
      }
    } yield ()
}