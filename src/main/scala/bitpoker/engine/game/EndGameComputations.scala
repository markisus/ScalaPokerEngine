package bitpoker.engine.game

import UtilityComputations._
import GameComputation._
import CallComputations._
import GameConstantComputations._
import BetComputations._
import TransitioningComputations._
import Utility._
import bitpoker.engine.cards.Card
import bitpoker.engine.handranking.Hand7
import bitpoker.engine.handranking.NaiveHandRefiner
import bitpoker.engine.handranking.Hand5
import bitpoker.engine.handranking.HandOrdering

object EndGameComputations {
  def showDown = {
    //do the showdown stuff
    for {
      _ <- resetGameResults
      usersStanding <- getUsersStanding
      sidePots <- calculateSidePots
      _ <- showDownWithContenders(usersStanding, sidePots)
      _ <- revealHoleCards
      //We will be resetting the game so let the clients know
      //what the final community cards were (which they would
      //not otherwise see when under showdown)
      communityCards <- getCommunityCards
      _ <- addGameResult(FinalCommunityCards(communityCards))
      _ <- resetGame
    } yield ()
  }

  def revealHoleCards =
    for {
      usersStanding <- getUsersStanding
      allHoleCards <- getUseridsToHoleCards
      revealedHoleCards = allHoleCards.filter(kvp =>
        usersStanding.contains(kvp._1))
      _ <- chain(revealedHoleCards.map(kvp =>
        addGameResult(RevealedHoleCards(kvp._1, kvp._2))))
    } yield ()

  def showDownWithContenders(
    remainingContenders: Set[Int],
    remainingSidePots: Seq[SidePot]): GameComputation[Unit] = {
    if (remainingSidePots.size == 0) {
      noop
    } else
      //Find the best hands
      for {
        useridsWithHand5s <- multi(
          remainingContenders
            .toSeq
            .map(userid => getHand(userid).map(userid -> _)))
        useridsToHand5s = useridsWithHand5s.toMap
        winners = findWinners(useridsToHand5s)
        useridsToTotalBets <- getUseridsToTotalBets
        sidePotsToWinners = remainingSidePots.map(
          sidePot => {
            sidePot ->
              winners.filter(winnerId =>
                useridsToTotalBets.get(winnerId).getOrElse(0) > sidePot.floor)
              .toSet
          }).toMap
        //side pots which somebody has won
        wonSidePotsToWinners = sidePotsToWinners.filter(kvp => kvp._2.size > 0)
        _ <- chain(wonSidePotsToWinners.map(kvp => awardSidePot(kvp._1, kvp._2, useridsToHand5s)))

        newContenders = remainingContenders.diff(winners.toSet)
        newSidePots = remainingSidePots.diff(wonSidePotsToWinners.keys.toSeq)
        _ <- showDownWithContenders(newContenders, newSidePots)
      } yield ()
  }

  def awardSidePot(sidePot: SidePot, winners: Set[Int], useridsToHand5s: Map[Int, Hand5]) = {
    val numWinners = winners.size
    val awardWinner = (userid: Int) =>
      for {
        userMoney <- getUserMoneyOrDefault(userid, 0)
        amountWon = sidePot.amount / numWinners
        _ <- incrementUserMoney(userid, amountWon)
        _ <- addGameResult(WonShowdown(
          userid = userid,
          amountWon = amountWon,
          potSize = sidePot.amount,
          hand = useridsToHand5s.get(userid).get))
      } yield ()

    chain(winners.map(awardWinner))
  }

  def findWinners(useridsToHands: Map[Int, Hand5]) = {
    val sorted = useridsToHands.toSeq.sortBy(t => t._2)(HandOrdering).reverse
    val bestHand = sorted.head._2
    sorted.takeWhile(t => HandOrdering.compare(t._2, bestHand) >= 0).map(t => t._1)
  }

  def getHand(userid: Int) =
    for {
      holeCards <- getHoleCards(userid).map(_.get)
      communityCards <- getCommunityCards
      hand7 = new Hand7(holeCards.toSet ++ communityCards)
    } yield NaiveHandRefiner.best5(hand7)

  def calculateSidePots = {
    for {
      usersStanding <- getUsersStanding
      totalBets <- getUseridsToTotalBets
      levels: Seq[Int] = Seq(0) ++ totalBets.filter(kvp =>
        usersStanding.contains(kvp._1))
        .map(_._2)
        .toSet.toList.sorted
      pots = levels.sliding(2).map(window => {
        val (floor, ceiling) = (window(0), window(1))
        val amount =
          totalBets.values.map(totalBet =>
            if (totalBet > ceiling)
              ceiling - floor
            else if (totalBet > floor)
              totalBet - floor
            else 0).sum
        new SidePot(
          floor = floor,
          ceiling = ceiling,
          amount = amount)
      }).toList

    } yield {
      pots
    }
  }

  def getLastManStanding = {
    for {
      usersStanding <- getUsersStanding
    } yield {
      if (usersStanding.size == 1)
        usersStanding.headOption
      else None
    }
  }

  private def getUsersStanding = {
    for {
      useridsToStates <- getUseridsToStates
      usersStanding = findKeysByValues(
        useridsToStates,
        (playerState: PlayerState) =>
          Set[PlayerState](AllIn, Ready).contains(playerState))
    } yield usersStanding
  }

  def awardLastManStanding(lastManStanding: Int) = {
    for {
      _ <- resetGameResults
      pot <- getPot
      money <- getUserMoneyOrDefault(lastManStanding)
      _ <- addGameResult(WonLastManStanding(lastManStanding))
      _ <- setUserMoney(lastManStanding, money + pot) andThen resetGame
    } yield ()
  }

  def resetGame = {
    for {
      _ <- setActiveUserPosition(0)
      seatedUserids <- getSeatedUserids
      foldedAndAllInUsers <- getUsersWithStates(Set(Folded, AllIn))
      bustedUsers <- filter(seatedUserids.toSeq, willUserBust)
      _ <- chain(seatedUserids.map(resetHoleCards(_)))
      _ <- chain(seatedUserids.map(resetTotalBet(_)))
      _ <- chain(foldedAndAllInUsers.map(setUserState(_, Ready)))
      _ <- chain(bustedUsers.map(setUserState(_, Busted)))
      _ <- resetCommunityCards
      _ <- resetMinRaise
      _ <- resetPot
      _ <- resetRoundVariables
      _ <- setGameState(WaitForPlayers)
    } yield ()
  }

  private def willUserBust(userid: Int) =
    for {
      userMoney <- getUserMoney(userid)
    } yield userMoney.map(_ == 0).getOrElse(false)

}