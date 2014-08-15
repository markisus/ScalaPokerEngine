package bitpoker.engine.game

import bitpoker.engine.game.typedefs._
import bitpoker.engine.cards.Card
import bitpoker.engine.game.denominations._
import UtilityComputations._
import JoinComputations._
import StartComputations._
import LeaveComputations._
import RebuyComputations._
import ForceBigBlindComputations._
import FoldComputations._
import RaiseAllInComputations._
import RebuyComputations._
import GameConstantComputations._
import GameComputation._
import CallComputations._
import BetComputations._
import bitpoker.engine.cards.DeckMaker

class GameDriver(
  denomination: Denomination,
  bigBlind: Long,
  smallBlind: Long,
  minBuyIn: Long,
  maxBuyIn: Long,
  onWaitingForMove: Userid => Unit = id => (),
  onMoveCompleted: Userid => Unit = id => (),
  queryFunds: (Userid, Denomination) => Long = (_, _) => Long.MaxValue,
  requestFunds: (Userid, Long, Denomination) => Boolean = (userid, amount, denomination) => true,
  releaseFunds: (Userid, Long, Denomination) => Unit = (userid, amount, denomination) => ()) {

  private val settings = GameSettings(
    numSeats = 10,
    minBuyIn = minBuyIn.toInt,
    maxBuyIn = maxBuyIn.toInt,
    smallBlind = smallBlind.toInt,
    bigBlind = bigBlind.toInt)

  private var game = new GameKernel(settings)

  private var previousActiveUserid = getActiveUserid.eval(game)
  private var previousGameState = getGameState.eval(game)
  private var gameJustEnded = false

  def getExternalFunds(userid: Int) =
    queryFunds(userid, denomination).toInt

  def actionPossibilities(userid: Int) =
    {
      val externalFunds = getExternalFunds(userid)
      val possibilities: Seq[ActionPossibility] = Seq(
        joinPossibility(userid, externalFunds).eval(game),
        startPossibility.eval(game),
        leavePossibility(userid).eval(game),
        rebuyPossibility(userid).eval(game),
        forceBigBlindPossibility(userid).eval(game),
        foldPossibility(userid).eval(game),
        raiseAllInPossibility(userid).eval(game),
        callPossibility(userid).eval(game),
        betPossibility(userid).eval(game))
        .flatMap({
          case Left(_) => Seq.empty
          case Right(possibility) => Seq(possibility)
        })
      possibilities
    }

  def getActions(userid: Userid): Seq[ActionPossibility] = actionPossibilities(userid)

  def makePlayerFacade(userid: Int): GameComputation[Option[PlayerFacade]] = {
    for {
      money <- getUserMoney(userid)
      state <- getUserState(userid)
      roundBet <- getRoundBet(userid)
      name <- getName(userid)
    } yield {
      for {
        m <- money
        s <- state
        r <- roundBet
        n <- name
      } yield PlayerFacade(userid, m, s, r, n)
    }
  }

  def makeSeatsFacade = {
    for {
      seatNumbersToUserids <- getSeatNumbersToUserids
      userids = seatNumbersToUserids.values.toSeq
      maybePlayerFacades <- multi(userids.map(makePlayerFacade(_)))
      playerFacades = maybePlayerFacades.flatMap(_.toSeq)
      useridsToPlayerFacades = playerFacades.map(p => p.userid -> p).toMap[Userid, PlayerFacade]
      seatNumbersToPlayerFacades = seatNumbersToUserids mapValues useridsToPlayerFacades
    } yield {
      SeatsFacade(seatNumbersToPlayerFacades)
    }
  }

  def makeFacade() = {
    val seatsFacade = makeSeatsFacade.eval(game)

    new GameFacade(
      numSeats = GameConstantComputations.getNumSeats.eval(game),
      numSeatsFilled = getSeatedUserids.map(_.size).eval(game),
      minBuyIn = getMinBuyIn.eval(game),
      maxBuyIn = getMaxBuyIn.eval(game),
      smallBlind = getSmallBlind.eval(game),
      bigBlind = getBigBlind.eval(game),
      dealerPosition = getDealerPosition.eval(game),
      smallBlindPosition = getSmallBlindPosition.eval(game),
      bigBlindPosition = getBigBlindPosition.eval(game),
      activeUserid = getActiveUserid.eval(game),
      communityCards = getCommunityCards.eval(game),
      seats = seatsFacade,
      state = getGameState.eval(game),
      pot = getPot.eval(game),
      results = UtilityComputations.getGameResults.eval(game),
      currentBet = getCurrentBet.eval(game),
      denomination = denomination.asInt)
  }

  def getGameFacade(): GameFacade = makeFacade()
  def getHoleCards(userid: Userid): Seq[Card] =
    UtilityComputations.getHoleCards(userid).eval(game).getOrElse(Seq.empty)
  def getNumSeats() = GameConstantComputations.getNumSeats.eval(game)
  def getNumPlayersSeated() = getSeatedUserids.map(_.size).eval(game)
  def getSeated(): Seq[Int] = getSeatedUserids.eval(game).toSeq
  def getUnseated(userids: Seq[Int]) = userids.filter(!getSeated().contains(_))

  def doAction(userid: Userid, action: Action): Either[ActionImpossibility, ActionSuccess] = {
    val computation = action match {
      case Start() => StartComputations.start(userid, () => DeckMaker.makeShuffledDeck)
      case Join(seatNumber, buyInAmount, name) => JoinComputations.join(
        userid,
        seatNumber,
        buyInAmount.toInt,
        name)
      case Leave() => LeaveComputations.leave(userid)
      case Rebuy(rebuyAmount) => RebuyComputations.rebuy(userid, rebuyAmount.toInt, getExternalFunds(userid))
      case ForceBigBlind() => ForceBigBlindComputations.forceBigBlind(userid)
      case Fold() => FoldComputations.fold(userid)
      case Call() => CallComputations.call(userid)
      case Bet(betAmount) => BetComputations.bet(userid, betAmount.toInt)
      case RaiseAllIn() => RaiseAllInComputations.raiseAllIn(userid)
    }

    val (newGame, result) =
      computation.computation(game)

    //check if active userid changed
    val oldActiveUserid = getActiveUserid.eval(game)
    val newActiveUserid = getActiveUserid.eval(newGame)

    val oldGameState = getGameState.eval(game)
    val newGameState = getGameState.eval(newGame)

    val hasActiveUseridChanged = oldActiveUserid != newActiveUserid
    val hasGameStateChanged = oldGameState != newGameState

    game = newGame

    if ((oldGameState != newGameState) && newGameState == WaitForPlayers)
      gameJustEnded = true
    else
      gameJustEnded = false

    if (oldActiveUserid.isDefined && (hasGameStateChanged || hasActiveUseridChanged)) {
      onMoveCompleted(oldActiveUserid.get)
    }
    if (newActiveUserid.isDefined && (hasGameStateChanged || hasActiveUseridChanged)) {
      onWaitingForMove(newActiveUserid.get)
    }

    result
  }

  def getUseridsInPlay =
    getUsersWithStates(Set(Ready, AllIn)).eval(game).toSeq

  def getGameResults() =
    UtilityComputations.getGameResults.eval(game)

  def didGameJustEnd(): Boolean = gameJustEnded

}

case class GameFacade(
  numSeats: Int,
  numSeatsFilled: Int,
  minBuyIn: Long,
  maxBuyIn: Long,
  smallBlind: Long,
  bigBlind: Long,
  dealerPosition: SeatNumber,
  smallBlindPosition: SeatNumber,
  bigBlindPosition: SeatNumber,
  activeUserid: Option[Userid],
  communityCards: Seq[Card],
  seats: SeatsFacade,
  state: GameState,
  pot: Long,
  results: GameResults,
  currentBet: Long,
  denomination: Int)