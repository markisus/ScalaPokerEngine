package bitpoker.engine.game
//Collection of utility game computations

import bitpoker.engine.cards.Card
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._

object UtilityComputations {
    
  def getDealerPosition: GameComputation[Int] =
    get(_.dealerPosition)

  def setDealerPosition(newDealerPosition: Int): GameComputation[Unit] =
    put(_.copy(dealerPosition = newDealerPosition))

  def getActiveUserid =
    for {
      activeUserPosition <- getActiveUserPosition
      seatNumbersToUserids <- getSeatNumbersToUserids
    } yield seatNumbersToUserids.get(activeUserPosition)

  def isUserActive(userid: Int) =
  	for {
  	  activeUserid <- getActiveUserid
  	} yield activeUserid.map(_ == userid).getOrElse(false)
  
  def getActiveUserPosition: GameComputation[Int] =
    get(_.activeUserPosition)

  def setActiveUserPosition(newActiveUserPosition: Int): GameComputation[Unit] =
    put(_.copy(activeUserPosition = newActiveUserPosition))

  def getPot: GameComputation[Int] =
    get(_.pot)

  def incrementPot(amount: Int): GameComputation[Int] =
    for {
      pot <- getPot
      increasedPot = pot + amount
      _ <- put(_.copy(pot = increasedPot))
    } yield increasedPot

  def resetPot: GameComputation[Unit] =
    put(_.copy(pot = 0))

  def getCurrentBet: GameComputation[Int] =
    for {
      roundBets <- getUseridsToRoundBets.map(_.values.toSet)
      currentBet = (roundBets + 0).max
    } yield currentBet

  def incrementCurrentBet(amount: Int): GameComputation[Int] =
    for {
      currentBet <- getCurrentBet
      increasedCurrentBet = currentBet + amount
      _ <- put(_.copy(currentBet = increasedCurrentBet))
    } yield increasedCurrentBet

  def resetCurrentBet: GameComputation[Unit] =
    put(_.copy(currentBet = 0))

  def getGameState: GameComputation[GameState] =
    get(_.gameState)

  def getDeck: GameComputation[Seq[Card]] =
    get(_.deck)

  def setDeck(deck: Seq[Card]): GameComputation[Unit] =
    put(_.copy(deck = deck))

  def deckPop(numToPop: Int): GameComputation[Seq[Card]] =
    for {
      deck <- getDeck
      (topCards, rest) = (deck.take(numToPop), deck.drop(numToPop))
      _ <- setDeck(rest)
    } yield topCards

  def setGameState(gameState: GameState): GameComputation[Unit] =
    put(_.copy(gameState = gameState))

  private def setCommunityCards(communityCards: Seq[Card]): GameComputation[Unit] =
    put(_.copy(communityCards = communityCards))

  def getCommunityCards: GameComputation[Seq[Card]] =
    get(_.communityCards)

  def resetCommunityCards: GameComputation[Unit] =
    put(_.copy(communityCards = Seq.empty))

  def dealCommunityCards(numCards: Int): GameComputation[Seq[Card]] =
    for {
      communityCards <- getCommunityCards
      newCards <- deckPop(numCards)
      updatedCards = communityCards ++ newCards
      _ <- setCommunityCards(updatedCards)
    } yield updatedCards

  def getSeatNumbersToUserids: GameComputation[Map[Int, Int]] =
    get(_.seatNumbersToUserids)

  def getSeatsWithStates(states: Set[PlayerState]) =
    for {
      seatNumbersToStates <- getSeatNumbersToStates
      seats = findKeysByValues[Int, PlayerState](
        seatNumbersToStates,
        states.contains(_))
    } yield seats

  def getUsersWithStates(states: Set[PlayerState]) =
    for {
      useridsToStates <- getUseridsToStates
      userids = findKeysByValues[Int, PlayerState](
        useridsToStates,
        states.contains(_))
    } yield userids

  def getNames(userids: Set[Int]) =
    for {
      useridsToStates <- getUseridsToNames
    } yield {
      useridsToStates
        .filter(kvp => userids.contains(kvp._1))
        .map(_._2)
    }

  def getUseridInSeatNumber(seatNumber: Int): GameComputation[Option[Int]] =
    get(_.seatNumbersToUserids.get(seatNumber))

  def getSeatNumbersToStates: GameComputation[Map[Int, PlayerState]] =
    for {
      useridsToStates <- getUseridsToStates
      seatNumbersToUserids <- getSeatNumbersToUserids
    } yield seatNumbersToUserids mapValues useridsToStates

  def getSeatNumbersOfUsers(userids: Set[Int]): GameComputation[Set[Int]] =
    for {
      seatNumbersToUserids <- getSeatNumbersToUserids
    } yield {
      findKeysByValues(seatNumbersToUserids,
        (userid: Int) => userids.contains(userid))
    }

  def preparedUserStates: Set[PlayerState] =
    Set(JustJoined, Ready, ForcingBigBlind)

  def getUserStates =
    getUseridsToStates.map(_.values.toSeq)

  def countUserStates(state: PlayerState): GameComputation[Int] =
    getUserStates.map(_.count(s => s == state))

  def countUserStates(states: Set[PlayerState]): GameComputation[Int] =
    getUserStates.map(_.count(s => states.contains(s)))

  def getSeatedUserids: GameComputation[Set[Int]] =
    get(_.seatNumbersToUserids.values.toSet)

  def getOccupiedSeatNumbers: GameComputation[Set[Int]] =
    get(_.seatNumbersToUserids.keys.toSet)

  private def getUseridsToMoney: GameComputation[Map[Int, Int]] =
    get(_.userMaps.useridsToMoney)

  def getUseridsToRoundBets: GameComputation[Map[Int, Int]] =
    get(_.userMaps.useridsToRoundBets)

  def getUseridsToTotalBets: GameComputation[Map[Int, Int]] =
    get(_.userMaps.useridsToTotalBets)

  def isUserSeated(userid: Int): GameComputation[Boolean] =
    get(_.isUserSeated(userid))

  def getSeatNumber(userid: Int): GameComputation[Option[Int]] =
    get(gameKernel =>
      flipMap(gameKernel.seatNumbersToUserids).get(userid))

  def getUserMoney(userid: Int): GameComputation[Option[Int]] =
    for {
      useridsToMoney <- getUseridsToMoney
    } yield useridsToMoney.get(userid)

  def getUserMoneyOrDefault(userid: Int, default: Int = 0) =
    getUserMoney(userid).map(_.getOrElse(default))

  def setUserMoney(userid: Int, amount: Int): GameComputation[Boolean] =
    doWhenUserSeated(userid)(
      for {
        useridsToMoney <- getUseridsToMoney
        updatedMoneys = useridsToMoney + (userid -> amount)
        _ <- put(k =>
          k.copy(userMaps = k.userMaps.copy(useridsToMoney = updatedMoneys)))
      } yield ())

  def incrementUserMoney(userid: Int, amount: Int): GameComputation[Boolean] =
    doWhenUserSeated(userid)(
      for {
        money <- getUserMoneyOrDefault(userid)
        _ <- setUserMoney(userid, money + amount)
      } yield ())

  def getUseridsToHoleCards: GameComputation[Map[Int, Seq[Card]]] =
    get(_.userMaps.useridsToHoleCards)

  def setHoleCards(userid: Int, cards: Seq[Card]): GameComputation[Boolean] =
    doWhenUserSeated(userid)(
      for {
        useridsToHoleCards <- getUseridsToHoleCards
        updatedHoleCards = useridsToHoleCards + (userid -> cards)
        _ <- put(k =>
          k.copy(userMaps = k.userMaps.copy(useridsToHoleCards = updatedHoleCards)))
      } yield ())

  def resetHoleCards(userid: Int): GameComputation[Boolean] =
    setHoleCards(userid, Seq.empty)

  def getHoleCards(userid: Int): GameComputation[Option[Seq[Card]]] =
    for {
      useridsToHoleCards <- getUseridsToHoleCards
    } yield useridsToHoleCards.get(userid)

  def getUseridsToNames: GameComputation[Map[Int, String]] =
    get(_.userMaps.useridsToNames)

  def getName(userid: Int): GameComputation[Option[String]] =
    get(_.userMaps.useridsToNames.get(userid))

  def getUseridsToStates: GameComputation[Map[Int, PlayerState]] =
    get(_.userMaps.useridsToStates)

  def setUserState(userid: Int, state: PlayerState): GameComputation[Boolean] =
    doWhenUserSeated(userid)(
      for {
        useridsToStates <- getUseridsToStates
        updatedStates = useridsToStates + (userid -> state)
        _ <- put(k => k.copy(userMaps = k.userMaps.copy(useridsToStates = updatedStates)))
      } yield ())

  def getUserState(userid: Int): GameComputation[Option[PlayerState]] = {
    for {
      useridsToStates <- getUseridsToStates
    } yield useridsToStates.get(userid)
  }

  def getRoundBet(userid: Int): GameComputation[Option[Int]] =
    for {
      useridsToRoundBets <- getUseridsToRoundBets
    } yield useridsToRoundBets.get(userid)

  def incrementRoundBet(userid: Int, amount: Int): GameComputation[Option[Int]] =
    for {
      currentRoundBet <- getRoundBet(userid)
      newRoundBet = currentRoundBet.map(_ + amount)
      _ <- newRoundBet.map(c => setRoundBet(userid, c))
        .getOrElse(noop)
    } yield newRoundBet

  def resetRoundBet(userid: Int): GameComputation[Boolean] =
    setRoundBet(userid, 0)

  def getTotalBet(userid: Int): GameComputation[Option[Int]] =
    for {
      useridsToTotalBets <- getUseridsToTotalBets
    } yield useridsToTotalBets.get(userid)

  def incrementTotalBet(userid: Int, amount: Int): GameComputation[Option[Int]] =
    for {
      currentTotalBet <- getTotalBet(userid)
      newTotalBet = currentTotalBet.map(_ + amount)
      _ <- newTotalBet.map(c => setTotalBet(userid, c))
        .getOrElse(noop)
    } yield newTotalBet

  def resetTotalBet(userid: Int): GameComputation[Boolean] =
    setTotalBet(userid, 0)

  private def setTotalBet(userid: Int, amount: Int): GameComputation[Boolean] = {
    doWhenUserSeated(userid)(put(gameKernel => {
      val updatedTotalBets =
        gameKernel.userMaps.useridsToTotalBets + (userid -> amount)
      gameKernel.copy(userMaps =
        gameKernel.userMaps.copy(useridsToTotalBets = updatedTotalBets))
    }))
  }

  private def setRoundBet(userid: Int, amount: Int): GameComputation[Boolean] = {
    doWhenUserSeated(userid)(put(gameKernel => {
      val updatedRoundBets =
        gameKernel.userMaps.useridsToRoundBets + (userid -> amount)
      gameKernel.copy(userMaps =
        gameKernel.userMaps.copy(useridsToRoundBets = updatedRoundBets))
    }))
  }

  def initializeUser(
    userid: Int,
    name: String,
    seatNumber: Int,
    money: Int): GameComputation[Unit] = {

    put(gameKernel => {
      lazy val updatedSeats =
        gameKernel.seatNumbersToUserids + (seatNumber -> userid)

      lazy val updatedHoleCards =
        gameKernel.userMaps.useridsToHoleCards + (userid -> Seq.empty)

      lazy val updatedMoneys =
        gameKernel.userMaps.useridsToMoney + (userid -> money)

      lazy val updatedRoundBets =
        gameKernel.userMaps.useridsToRoundBets + (userid -> 0)

      lazy val updatedNames =
        gameKernel.userMaps.useridsToNames + (userid -> name)

      lazy val updatedUserStates =
        gameKernel.userMaps.useridsToStates + (userid -> JustJoined)

      lazy val updatedTotalBets =
        gameKernel.userMaps.useridsToTotalBets + (userid -> 0)

      if (gameKernel.isUserSeated(userid))
        //Initializing a user who is already in game is a no-op
        gameKernel
      else {
        lazy val updatedUserMaps =
          gameKernel.userMaps.copy(
            useridsToHoleCards = updatedHoleCards,
            useridsToMoney = updatedMoneys,
            useridsToRoundBets = updatedRoundBets,
            useridsToNames = updatedNames,
            useridsToStates = updatedUserStates,
            useridsToTotalBets = updatedTotalBets)

        gameKernel.copy(
          seatNumbersToUserids = updatedSeats,
          userMaps = updatedUserMaps)
      }
    })
  }

  def kickUser(userid: Int): GameComputation[Unit] = {
    put(gameKernel => {
      val useridsToSeatNumbers =
        flipMap(gameKernel.seatNumbersToUserids)

      lazy val updatedSeats =
        useridsToSeatNumbers.get(userid)
          .map(seatNumber => gameKernel.seatNumbersToUserids - seatNumber)
          .getOrElse(gameKernel.seatNumbersToUserids)

      lazy val updatedHoleCards =
        gameKernel.userMaps.useridsToHoleCards - userid

      lazy val updatedMoneys =
        gameKernel.userMaps.useridsToMoney - userid

      lazy val updatedRoundBets =
        gameKernel.userMaps.useridsToRoundBets - userid

      lazy val updatedNames =
        gameKernel.userMaps.useridsToNames - userid

      lazy val updatedUserStates =
        gameKernel.userMaps.useridsToStates - userid

      lazy val updatedTotalBets =
        gameKernel.userMaps.useridsToTotalBets - userid

      gameKernel.copy(
        seatNumbersToUserids = updatedSeats,

        userMaps = gameKernel.userMaps.copy(
          useridsToHoleCards = updatedHoleCards,
          useridsToMoney = updatedMoneys,
          useridsToRoundBets = updatedRoundBets,
          useridsToNames = updatedNames,
          useridsToStates = updatedUserStates,
          useridsToTotalBets = updatedTotalBets))
    })
  }

  def getMinRaise =
    get(_.minRaise)

  def setMinRaise(newMinRaise: Int) =
    put(_.copy(minRaise = newMinRaise))

  def resetMinRaise =
    setMinRaise(0)

  def getUsersWhoHadTurn =
    get(_.usersWhoHadTurn)

  def resetUsersWhoHadTurn =
    put(_.copy(usersWhoHadTurn = Set.empty))

  def addToUsersWhoHadTurn(userid: Int) =
    for {
      usersWhoHadTurn <- getUsersWhoHadTurn
      updatedUsersWhoHadTurn = usersWhoHadTurn + userid
      _ <- put(_.copy(usersWhoHadTurn = updatedUsersWhoHadTurn))
    } yield updatedUsersWhoHadTurn

  def getBigBlindPosition =
    get(_.bigBlindPosition)

  def setBigBlindPosition(seatNumber: Int) =
    put(_.copy(bigBlindPosition = seatNumber))

  def getSmallBlindPosition =
    get(_.smallBlindPosition)

  def setSmallBlindPosition(seatNumber: Int) =
    put(_.copy(smallBlindPosition = seatNumber))

  def addGameResult(result: Result) =
    put(k =>
      k.copy(results = k.results.withResult(result)))

  def resetGameResults =
    put(_.copy(results = GameResults()))

  def getGameResults =
    get(_.results)

  def doWhenUserSeated(
    userid: Int)(computation: GameComputation[Unit]): GameComputation[Boolean] =
    for {
      userSeated <- isUserSeated(userid)
      result <- if (userSeated) {
        for {
          _ <- computation
        } yield true //Executed computation b/c user was seated
      } else emit(false) //User was not seated
    } yield result
}