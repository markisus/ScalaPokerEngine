package bitpoker.engine.game

import UtilityComputations._
import GameComputation._
import CallComputations._
import GameConstantComputations._
import BetComputations._
import TransitioningComputations._

import Utility._
import bitpoker.engine.cards.Card

object MoveComputations {
  def possibleIfUserActive[P <: ActionPossibility](
    userid: Int,
    possibility: GameComputation[Either[ActionImpossibility, P]]): GameComputation[Either[ActionImpossibility, P]] = {

    for {
      userActive <- isUserActive(userid)
      result <- {
        if (userActive)
          possibility
        else emit(Left(NotThisUsersTurn()))
      }
    } yield result
  }

  def makeMoveComputation[I <: ActionImpossibility, P <: ActionPossibility, S <: ActionSuccess](
    userid: Int,
    possibility: GameComputation[Either[I, P]],
    action: P => GameComputation[Either[I, S]]): GameComputation[Either[I, S]] = {
    for {
      possibilityResult <- possibility
      result <- possibilityResult
        .fold[GameComputation[Either[I, S]]](
          impossibility => emit(impossibility).map(Left(_)),
          possibility => action(possibility))

      //Post action
      _ <- result.fold(
        impossiblity => noop,
        success => {
          addToUsersWhoHadTurn(userid) andThen endTurn
        })
    } yield result
  }
}