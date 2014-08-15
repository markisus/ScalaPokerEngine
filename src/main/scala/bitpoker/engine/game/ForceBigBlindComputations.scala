package bitpoker.engine.game

import bitpoker.engine.game.GameConstantComputations._
import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._

object ForceBigBlindComputations {

  def forceBigBlindPossibility(
    userid: Int): GameComputation[Either[ActionImpossibility, CanForceBigBlind]] =
    for {
      userState <- getUserState(userid)
      canForceBigBlind = userState.map(_ == JustJoined).getOrElse(false)
      result <- {
        if (canForceBigBlind)
          emit(CanForceBigBlind()).map(Right(_))
        else
          emit(NotJustJoinedCannotForceBigBlind()).map(Left(_))
      }
    } yield result

  def forceBigBlind(userid: Int): GameComputation[Either[ActionImpossibility, ActionSuccess]] = {
    for {
      possibility <- forceBigBlindPossibility(userid)
      result <- possibility.fold(
        impossiblity => emit(impossiblity).map(Left(_)),
        canForceBigBlind =>
          setUserState(userid, ForcingBigBlind)
            .andThen(emit(PlayerForcedBigBlind(userid)).map(Right(_))))
    } yield result
  }
}