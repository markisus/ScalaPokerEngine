package bitpoker.engine.game

import bitpoker.engine.game.GameConstantComputations._
import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._
import MoveComputations._

object FoldComputations {
  def foldPossibility(
    userid: Int): GameComputation[Either[ActionImpossibility, CanFold]] =
    possibleIfUserActive(userid,
      emit(CanFold()).map(Right(_)))

  def fold(
    userid: Int): GameComputation[Either[ActionImpossibility, PlayerFolded]] =

    makeMoveComputation[ActionImpossibility, CanFold, PlayerFolded](
      userid,
      foldPossibility(userid),
      canFold =>
        setUserState(userid, Folded) andThen (emit(PlayerFolded(userid)).map(Right(_))))
}