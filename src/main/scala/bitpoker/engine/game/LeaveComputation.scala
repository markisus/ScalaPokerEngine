package bitpoker.engine.game

import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._
import bitpoker.engine.game.UtilityComputations._
import EndGameComputations._

object LeaveComputations {
  def leavePossibility(
    userid: Int): GameComputation[Either[ActionImpossibility, CanLeave]] =
    isUserSeated(userid).map(userSeated =>
      if (userSeated) Right(CanLeave()) else Left(NotInGame()))

  def leave(userid: Int): GameComputation[Either[ActionImpossibility, ActionSuccess]] =
    for {
      possibility <- leavePossibility(userid)
      result <- possibility
        .fold[GameComputation[Either[ActionImpossibility, ActionSuccess]]](
          impossibility => emit(Left(impossibility)),
          //Award last man standing
          canLeave =>
            for {
              _ <- kickUser(userid)
              maybeLastManStanding <- getLastManStanding
              _ <- maybeLastManStanding.map(awardLastManStanding(_)).getOrElse(noop)
            } yield Right(PlayerLeft(userid)))
    } yield result
}