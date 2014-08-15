package bitpoker.engine.game

import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.game.GameConstantComputations._
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._
import TransitioningComputations._
import BetComputations._
import bitpoker.engine.cards.Card
import bitpoker.engine.cards.DeckMaker

object StartComputations {

  val startPossibility: GameComputation[Either[ActionImpossibility, CanStart]] =
    for {
      gameState <- getGameState
      numPreparedUsers <- countUserStates(preparedUserStates)
      result <- if (gameState != WaitForPlayers)
        emit(Left(GameAlreadyStarted()))
      else if (numPreparedUsers < 2)
        emit(Left(NotEnoughPreparedUsers()))
      else
        emit(Right(CanStart()))
    } yield result

  def start(
    userid: Int,
    makeDeck: () => Seq[Card] = () => DeckMaker.makeShuffledDeck): GameComputation[Either[ActionImpossibility, ActionSuccess]] = {
    for {
      possibility <- startPossibility
      name <- getName(userid)
      result <- possibility.fold[GameComputation[Either[ActionImpossibility, ActionSuccess]]](
        impossibility => emit(Left(impossibility)),
        canStart => startGame(userid, makeDeck()).map(Right(_)))
    } yield result
  }

  private def startGame(userid: Int, deck: Seq[Card]): GameComputation[ActionSuccess] = {
    for {
      _ <- readyMorePlayers
      _ <- setDealer
      _ <- setBlindPositions
      _ <- setDeck(deck)
      _ <- startNextPhase
      _ <- dealAllHoleCards
      blindPayers <- collectBlinds
      smallBlindPayer = blindPayers._1
      bigBlindPayers = blindPayers._2
      smallBlindName <- getName(smallBlindPayer)
      bigBlindNames <- getNames(bigBlindPayers)
      startingUserName <- getName(userid)
    } yield {
      Started(
        startingUserName.getOrElse("No Name"),
        smallBlindName.getOrElse("No Name"), bigBlindNames.toSeq)
    }
  }

  private def setDealer: GameComputation[Unit] = {
    for {
      currentDealerPosition <- getDealerPosition
      readySeats <- getSeatsWithStates(Set(Ready))
      nextDealerPosition = nextGreatest(currentDealerPosition, readySeats)
      _ <- setDealerPosition(nextDealerPosition)
    } yield ()
  }

  def setBlindPositions: GameComputation[(Int, Int)] = {
    for {
      blindPositions <- computeBlindPositionsFromDealerPosition
      (smallBlindPosition, bigBlindPosition) = blindPositions
      _ <- setSmallBlindPosition(smallBlindPosition)
      _ <- setBigBlindPosition(bigBlindPosition)
    } yield (smallBlindPosition, bigBlindPosition)
  }

  //Returns userids of small blind payer, big blind payers, sets blind positions
  def collectBlinds: GameComputation[(Int, Set[Int])] = {
    for {
      smallBlind <- getSmallBlind
      bigBlind <- getBigBlind
      smallBlindPosition <- getSmallBlindPosition
      bigBlindPosition <- getBigBlindPosition
      smallBlindUserid <- getUseridInSeatNumber(smallBlindPosition)
      bigBlindUserid <- getUseridInSeatNumber(bigBlindPosition)
      usersForcingBigBlind <- getUsersWithStates(Set(ForcingBigBlind))
      _ <- smallBlindUserid.map(betOrAllIn(_, smallBlind)).getOrElse(noop)
      bigBlinders = bigBlindUserid.toSet ++ usersForcingBigBlind
      _ <- chain(bigBlinders.toSeq.map(betOrAllIn(_, bigBlind)))
    } yield (smallBlindUserid.getOrElse(0), bigBlinders)
  }

  def computeBlindPositionsFromDealerPosition: GameComputation[(Int, Int)] = {
    for {
      dealerPosition <- getDealerPosition
      smallBlindPotentials <- getSeatsWithStates(Set(Ready))
      bigBlindPotentials <- getSeatsWithStates(Set(
        Ready,
        JustJoined,
        ForcingBigBlind))

      smallBlindPosition = nextGreatest(
        dealerPosition,
        smallBlindPotentials)

      bigBlindPosition = nextGreatest(
        smallBlindPosition,
        bigBlindPotentials)

    } yield {
      //If the normal big blind position is the dealer position,
      //we are actually in heads up mode b/c we only have 2 players
      //and then we swap small and big blind positions
      if (bigBlindPosition == dealerPosition)
        (bigBlindPosition, smallBlindPosition)
      else
        (smallBlindPosition, bigBlindPosition)
    }
  }

  private def readyMorePlayers: GameComputation[Unit] = {
    for {
      numReadyUsers <- countUserStates(Ready)
      userStates <- getUseridsToStates
      _ <- if (numReadyUsers <= 1) {
        val readiableUsers = findKeysByValues[Int, PlayerState](
          userStates,
          preparedUserStates.contains(_))
        chain(readiableUsers.toSeq.map(setUserState(_, Ready)))
      } else noop
    } yield ()
  }

  private def dealAllHoleCards =
    for {
      readyUsers <- getUsersWithStates(Set(Ready, AllIn))
      _ <- chain(readyUsers.map(dealHoleCards(_)))
    } yield ()

  private def dealHoleCards(userid: Int): GameComputation[Unit] =
    for {
      cards <- deckPop(2)
      _ <- setHoleCards(userid, cards)
    } yield ()
}