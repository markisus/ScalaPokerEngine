package bitpoker.engine.game

import bitpoker.engine.game.typedefs._
import bitpoker.engine.handranking.Hand5
import bitpoker.engine.cards.Card

trait Result
case class RevealedHoleCards(userid: Userid, holeCards: Seq[Card]) extends Result
case class FinalCommunityCards(cards: Seq[Card]) extends Result
case class WonLastManStanding(userid: Userid) extends Result
case class WonShowdown(userid: Userid, amountWon: Long, potSize: Long, hand: Hand5) extends Result

case class GameResults(val results: Seq[Result] = Seq.empty) {
  def withResult(result: Result) = this.copy(results :+ result)
}