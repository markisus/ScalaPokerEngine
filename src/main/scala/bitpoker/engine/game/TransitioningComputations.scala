package bitpoker.engine.game

import UtilityComputations._
import GameComputation._
import CallComputations._
import GameConstantComputations._
import BetComputations._
import EndGameComputations._
import Utility._
import bitpoker.engine.cards.Card

object TransitioningComputations {

  def startNextPhase: GameComputation[Unit] = {
    val advanceGameState = for {
      _ <- setBettingState
      _ <- dealCards
      usersReady <- getUsersWithStates(Set(Ready))
      numReadyUsers = usersReady.size
      utgPosition <- setUtg
      _ <- if (numReadyUsers < 2)
        startNextPhase
      else noop
    } yield ()

    for {
      _ <- resetRoundVariables
      gameState <- getGameState
      _ <- if (gameState == River) {
        showDown
      } else
        advanceGameState
    } yield ()
  }

  def endTurn =
    for {
      //Check for last man standing
      maybeLastManStanding <- getLastManStanding
      _ <- maybeLastManStanding match {
        case Some(lastManStanding) => awardLastManStanding(lastManStanding)
        case None => {
          for {
            activeUserPosition <- advanceActiveUserPosition
            _ <- if (activeUserPosition == 0) //No one can move
              startNextPhase
            else noop
          } yield ()
        }
      }
    } yield ()

  def resetRoundVariables: GameComputation[Unit] =
    for {
      seatedUserids <- getSeatedUserids
      _ <- chain(seatedUserids.toSeq.map(resetRoundBet(_)))
      _ <- resetUsersWhoHadTurn
    } yield ()

  def setBettingState =
    for {
      currentState <- getGameState
      _ <- currentState match {
        case WaitForPlayers => setGameState(PreFlop)
        case PreFlop => setGameState(Flop)
        case Flop => setGameState(Turn)
        case Turn => setGameState(River)
        case _ => noop
      }
    } yield ()

  def dealCards =
    for {
      currentGameState <- getGameState
      _ <- currentGameState match {
        case Flop => dealCommunityCards(3)
        case Turn => dealCommunityCards(1)
        case River => dealCommunityCards(1)
        case _ => noop
      }
    } yield ()

  def setUtg =
    for {
      seatedUserids <- getSeatedUserids
      possibleUtgs <- filter(seatedUserids.toSeq, userCanBetActive)
      possibleUtgSeats <- getSeatNumbersOfUsers(possibleUtgs.toSet)
      bigBlindPosition <- getBigBlindPosition
      utgPosition = nextGreatest(bigBlindPosition, possibleUtgSeats)
      _ <- setActiveUserPosition(utgPosition)
    } yield utgPosition

  private def advanceActiveUserPosition =
    for {
      currentActiveUserPosition <- getActiveUserPosition
      seatedUserids <- getSeatedUserids
      possibleNextActiveUserids <- filter(seatedUserids.toSeq, userCanBetActive)
      possibleNextActivePositions <- getSeatNumbersOfUsers(possibleNextActiveUserids.toSet)
      nextActiveUserPosition = nextGreatest(
        currentActiveUserPosition,
        possibleNextActivePositions,
        0)
      _ <- setActiveUserPosition(nextActiveUserPosition)
    } yield nextActiveUserPosition

  private def userCanBetActive(userid: Int): GameComputation[Boolean] = {
    for {
      userState <- getUserState(userid).map(_.get)
      userRoundBet <- getRoundBet(userid).map(_.get)
      currentBet <- getCurrentBet
      hadTurn <- getUsersWhoHadTurn.map(_.contains(userid))
      name <- getName(userid)
    } yield (userState == Ready) && (userRoundBet < currentBet || !hadTurn)
  }
}