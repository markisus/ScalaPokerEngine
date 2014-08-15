package bitpoker.engine.game

import bitpoker.engine.game.GameConstantComputations._
import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.game.GameComputation._
import bitpoker.engine.game.Utility._

object JoinComputations {
  def joinPossibility(
    userid: Int,
    externalMoney: Int = Int.MaxValue): GameComputation[Either[ActionImpossibility, CanJoin]] =
    for {
      numSeats <- getNumSeats
      seatNumbers = Range(1, numSeats + 1).toSet
      occupiedSeats <- getOccupiedSeatNumbers
      emptySeats = seatNumbers diff occupiedSeats
      userSeated <- isUserSeated(userid)
      minBuyIn <- getMinBuyIn
      maxBuyIn <- getMaxBuyIn
      result <- if (externalMoney < minBuyIn)
        emit(NotEnoughMoney()).map(Left(_))
      else if (userSeated)
        emit(UserAlreadyInGame()).map(Left(_))
      else if (emptySeats.isEmpty)
        emit(GameIsFull()).map(Left(_))
      else emit(CanJoin(emptySeats, minBuyIn, maxBuyIn)).map(Right(_))
    } yield result

  def join(
    userid: Int,
    seatNumber: Int,
    buyInAmount: Int,
    name: String,
    externalMoney: Int = Int.MaxValue): GameComputation[Either[ActionImpossibility, ActionSuccess]] = {
    for {
      possibility <- joinPossibility(userid, externalMoney)
      result <- possibility
        .fold[GameComputation[Either[ActionImpossibility, ActionSuccess]]](
          impossibility => emit(Left(impossibility)),
          canJoin => {
            if (!canJoin.emptySeats.contains(seatNumber))
              emit(Left(SeatTaken()))
            else if (!isInRange(buyInAmount, canJoin.minBuyIn, canJoin.maxBuyIn))
              emit(Left(BuyInOutOfRange()))
            else
              initializeUser(userid, name, seatNumber, buyInAmount).andThen(
                emit(Right(PlayerJoined(userid, seatNumber, buyInAmount, name))))
          })
    } yield result
  }
}