package bitpoker.engine.handranking

import bitpoker.engine.cards.Rank
import bitpoker.engine.cards.Card

abstract class Hand5(val cards: Set[Card]) {
  require(cards.size == 5)
  def name: String
}
class Hand7(val cards: Set[Card]) {
  require(cards.size == 7)
}

case class RoyalFlush(
  override val cards: Set[Card]) extends Hand5(cards) {
  def name = {
    "Royal Flush"
  }
}

case class StraightFlush(
  override val cards: Set[Card],
  highRank: Rank.Value) extends Hand5(cards) {
  def name = {
    "Straight Flush " + highRank + " High"
  }
}

case class Quads(
  override val cards: Set[Card],
  quadRank: Rank.Value,
  otherRank: Rank.Value) extends Hand5(cards) {
  def name =
    "Quad " + quadRank + " with " + otherRank + " High"
}

case class FullHouse(
  override val cards: Set[Card],
  tripRank: Rank.Value,
  doubleRank: Rank.Value) extends Hand5(cards) {
  def name = "Full House " + tripRank + " Full of " + doubleRank
}

case class Flush(
  override val cards: Set[Card],
  ranksDescending: Seq[Rank.Value]) extends Hand5(cards) {
  def name = "Flush " + ranksDescending.head + " High"
}

case class Straight(
  override val cards: Set[Card],
  highRank: Rank.Value) extends Hand5(cards) {
  def name = "Straight " + highRank + " High"
}

/* Three of a Kind turns out to be weird
 * in that: for two Three of a Kinds, if the
 * kickers tie, no further comparisons are 
 * done and the hands are tied
 * 
 * Reference:
 * http://www.bigslickpokerplayer.com/tiebreakers.shtml
 */
case class ThreeOfAKind(
  override val cards: Set[Card],
  tripRank: Rank.Value,
  kickerRank: Rank.Value) extends Hand5(cards) {
  def name = "Three of a Kind of " + tripRank + " with " + kickerRank + " Kicker"
}

case class TwoPair(
  override val cards: Set[Card],
  highPairRank: Rank.Value,
  lowPairRank: Rank.Value,
  otherRank: Rank.Value)
  extends Hand5(cards) {
  def name = "Two Pair of " + highPairRank + " and " + lowPairRank
}

case class OnePair(
  override val cards: Set[Card],
  pairRank: Rank.Value,
  remainingRanksDescending: Seq[Rank.Value]) extends Hand5(cards) {
  def name = "One Pair of " + pairRank
}

case class HighCard(
  override val cards: Set[Card],
  ranksDescending: Seq[Rank.Value]) extends Hand5(cards) {
  def name = ranksDescending.head + " High"
}
