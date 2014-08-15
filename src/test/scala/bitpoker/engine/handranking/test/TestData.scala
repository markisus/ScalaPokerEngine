package bitpoker.engine.handranking.test

import bitpoker.engine.handranking.TwoPair
import bitpoker.engine.cards.Rank
import bitpoker.engine.handranking.HighCard
import bitpoker.engine.handranking.FullHouse
import bitpoker.engine.handranking.Hand7
import bitpoker.engine.handranking.Flush
import bitpoker.engine.handranking.StraightFlush
import bitpoker.engine.handranking.Quads
import bitpoker.engine.handranking.OnePair
import bitpoker.engine.handranking.ThreeOfAKind
import bitpoker.engine.cards.Suit
import bitpoker.engine.handranking.Straight
import bitpoker.engine.handranking.RoyalFlush
import bitpoker.engine.cards.Card

object TestDecks {
  val shuffledDeck1 = Seq(
    Card(Rank.Jack, Suit.Spades), Card(Rank.Five, Suit.Hearts),
    Card(Rank.Seven, Suit.Clubs), Card(Rank.Two, Suit.Clubs),
    Card(Rank.Jack, Suit.Hearts), Card(Rank.Six, Suit.Clubs),
    Card(Rank.Queen, Suit.Spades), Card(Rank.Ten, Suit.Diamonds),
    Card(Rank.Five, Suit.Clubs), Card(Rank.Seven, Suit.Diamonds),
    Card(Rank.Seven, Suit.Spades), Card(Rank.Queen, Suit.Hearts),
    Card(Rank.Three, Suit.Spades), Card(Rank.Ace, Suit.Clubs),
    Card(Rank.Eight, Suit.Diamonds), Card(Rank.Ten, Suit.Clubs),
    Card(Rank.King, Suit.Clubs), Card(Rank.Two, Suit.Diamonds),
    Card(Rank.Six, Suit.Spades), Card(Rank.Three, Suit.Hearts),
    Card(Rank.Four, Suit.Hearts), Card(Rank.Two, Suit.Spades),
    Card(Rank.King, Suit.Diamonds), Card(Rank.Four, Suit.Spades),
    Card(Rank.Jack, Suit.Diamonds), Card(Rank.Ace, Suit.Spades),
    Card(Rank.Ten, Suit.Spades), Card(Rank.King, Suit.Hearts),
    Card(Rank.Nine, Suit.Diamonds), Card(Rank.Ace, Suit.Hearts),
    Card(Rank.Queen, Suit.Diamonds), Card(Rank.Ace, Suit.Diamonds),
    Card(Rank.Nine, Suit.Hearts), Card(Rank.Two, Suit.Hearts),
    Card(Rank.Three, Suit.Clubs), Card(Rank.Ten, Suit.Hearts),
    Card(Rank.King, Suit.Spades), Card(Rank.Eight, Suit.Hearts),
    Card(Rank.Six, Suit.Diamonds), Card(Rank.Queen, Suit.Clubs),
    Card(Rank.Three, Suit.Diamonds), Card(Rank.Five, Suit.Spades),
    Card(Rank.Four, Suit.Clubs), Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Six, Suit.Hearts), Card(Rank.Nine, Suit.Spades),
    Card(Rank.Eight, Suit.Spades), Card(Rank.Four, Suit.Diamonds),
    Card(Rank.Seven, Suit.Hearts), Card(Rank.Five, Suit.Diamonds),
    Card(Rank.Eight, Suit.Clubs), Card(Rank.Nine, Suit.Clubs))
    
    require(shuffledDeck1.toSet.size == 52,
        "The test deck must have 52 cards")
}

object TestHand7s {
  val royalFlush7a = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.King, Suit.Clubs),
    Card(Rank.Queen, Suit.Clubs),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Ten, Suit.Clubs),
    Card(Rank.Two, Suit.Hearts),
    Card(Rank.Seven, Suit.Diamonds)))

  val royalFlush7b = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.King, Suit.Clubs),
    Card(Rank.Queen, Suit.Clubs),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Ten, Suit.Clubs),
    Card(Rank.Two, Suit.Hearts),
    Card(Rank.Seven, Suit.Diamonds)))

  val quad7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.Ace, Suit.Diamonds),
    Card(Rank.Ace, Suit.Hearts),
    Card(Rank.Ace, Suit.Spades),
    Card(Rank.Ten, Suit.Clubs),
    Card(Rank.Two, Suit.Hearts),
    Card(Rank.Jack, Suit.Diamonds)))

  val straightFlush7 = new Hand7(Set(
    Card(Rank.King, Suit.Diamonds),
    Card(Rank.Queen, Suit.Diamonds),
    Card(Rank.Jack, Suit.Diamonds),
    Card(Rank.Ten, Suit.Diamonds),
    Card(Rank.Nine, Suit.Diamonds),
    Card(Rank.King, Suit.Hearts),
    Card(Rank.King, Suit.Spades)))

  val fullHouse7 = new Hand7(Set(
    Card(Rank.King, Suit.Diamonds),
    Card(Rank.Queen, Suit.Diamonds),
    Card(Rank.Queen, Suit.Clubs),
    Card(Rank.Ten, Suit.Clubs),
    Card(Rank.Nine, Suit.Diamonds),
    Card(Rank.King, Suit.Hearts),
    Card(Rank.King, Suit.Spades)))

  val flush7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Diamonds),
    Card(Rank.Queen, Suit.Diamonds),
    Card(Rank.Three, Suit.Diamonds),
    Card(Rank.Ten, Suit.Clubs),
    Card(Rank.Nine, Suit.Diamonds),
    Card(Rank.King, Suit.Diamonds),
    Card(Rank.King, Suit.Clubs)))

  val straight7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.King, Suit.Diamonds),
    Card(Rank.Queen, Suit.Diamonds),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Ten, Suit.Diamonds),
    Card(Rank.King, Suit.Hearts),
    Card(Rank.King, Suit.Spades)))

  val trips7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.Ace, Suit.Diamonds),
    Card(Rank.Ace, Suit.Hearts),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Ten, Suit.Diamonds),
    Card(Rank.Two, Suit.Diamonds),
    Card(Rank.King, Suit.Spades)))

  val twoPair7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.Ace, Suit.Diamonds),
    Card(Rank.Five, Suit.Hearts),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Jack, Suit.Diamonds),
    Card(Rank.Two, Suit.Diamonds),
    Card(Rank.King, Suit.Diamonds)))

  val pair7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.Ace, Suit.Diamonds),
    Card(Rank.Eight, Suit.Hearts),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Ten, Suit.Diamonds),
    Card(Rank.Two, Suit.Diamonds),
    Card(Rank.King, Suit.Diamonds)))

  val high7 = new Hand7(Set(
    Card(Rank.Ace, Suit.Clubs),
    Card(Rank.Nine, Suit.Diamonds),
    Card(Rank.Four, Suit.Hearts),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.Ten, Suit.Clubs),
    Card(Rank.Two, Suit.Diamonds),
    Card(Rank.King, Suit.Spades)))
}

object TestHand5s {
  def makeRoyalFlush(suit: Suit.Value = Suit.Clubs): RoyalFlush = {
    RoyalFlush(Set(
      Card(Rank.Ace, suit),
      Card(Rank.King, suit),
      Card(Rank.Queen, suit),
      Card(Rank.Jack, suit),
      Card(Rank.Ten, suit)))
  }

  def makeStraightFlush(highRank: Rank.Value, suit: Suit.Value = Suit.Clubs) = {
    val rank1 = highRank
    val rank2 = Rank.previous(rank1)
    val rank3 = Rank.previous(rank2)
    val rank4 = Rank.previous(rank3)
    val rank5 = Rank.previous(rank4)

    val cards = Set(
      Card(rank1, suit),
      Card(rank2, suit),
      Card(rank3, suit),
      Card(rank4, suit),
      Card(rank5, suit))

    StraightFlush(cards, highRank)
  }

  def makeQuads(quadRank: Rank.Value, otherRank: Rank.Value) = {
    val cards = Set(
      Card(quadRank, Suit.Clubs),
      Card(quadRank, Suit.Diamonds),
      Card(quadRank, Suit.Spades),
      Card(quadRank, Suit.Hearts),
      Card(otherRank, Suit.Hearts))

    Quads(cards, quadRank, otherRank)
  }

  def makeFullHouse(tripRank: Rank.Value, doubleRank: Rank.Value) = {
    val cards = Set(
      Card(tripRank, Suit.Clubs),
      Card(tripRank, Suit.Hearts),
      Card(tripRank, Suit.Diamonds),
      Card(doubleRank, Suit.Hearts),
      Card(doubleRank, Suit.Spades))

    FullHouse(cards, tripRank, doubleRank)
  }

  def makeFlush(ranksDescending: Seq[Rank.Value]) = {
    val cards = ranksDescending.map(Card(_, Suit.Hearts)).toSet

    Flush(cards, ranksDescending)
  }

  def makeStraight(highRank: Rank.Value) = {
    val rank1 = highRank
    val rank2 = Rank.previous(rank1)
    val rank3 = Rank.previous(rank2)
    val rank4 = Rank.previous(rank3)
    val rank5 = Rank.previous(rank4)

    val cards = Set(
      Card(rank1, Suit.Clubs),
      Card(rank2, Suit.Hearts),
      Card(rank3, Suit.Clubs),
      Card(rank4, Suit.Diamonds),
      Card(rank5, Suit.Spades))

    Straight(cards, highRank)
  }

  def makeThreeOfAKind(tripRank: Rank.Value, kickerRank: Rank.Value) = {
    val dummyCardRank = (Rank.values.toSet - tripRank).filter(_ <= kickerRank).head
    val cards = Set(
      Card(tripRank, Suit.Clubs),
      Card(tripRank, Suit.Diamonds),
      Card(tripRank, Suit.Hearts),
      Card(kickerRank, Suit.Hearts),
      Card(dummyCardRank, Suit.Diamonds))

    ThreeOfAKind(cards, tripRank, kickerRank)
  }

  def makeTwoPair(highPairRank: Rank.Value, lowPairRank: Rank.Value, otherRank: Rank.Value) = {
    val cards = Set(
      Card(highPairRank, Suit.Clubs),
      Card(highPairRank, Suit.Hearts),
      Card(lowPairRank, Suit.Clubs),
      Card(lowPairRank, Suit.Diamonds),
      Card(otherRank, Suit.Spades))

    TwoPair(cards, highPairRank, lowPairRank, otherRank)
  }

  def makeOnePair(pairRank: Rank.Value, otherRanksDescending: Seq[Rank.Value]) = {
    val cards = Set(
      Card(pairRank, Suit.Clubs),
      Card(pairRank, Suit.Hearts),
      Card(otherRanksDescending(0), Suit.Diamonds),
      Card(otherRanksDescending(1), Suit.Hearts),
      Card(otherRanksDescending(2), Suit.Clubs))

    OnePair(cards, pairRank, otherRanksDescending)
  }

  def makeHighCard(ranksDescending: Seq[Rank.Value]) = {
    val suits = Seq(Suit.Clubs, Suit.Diamonds, Suit.Hearts, Suit.Spades, Suit.Diamonds)
    val cards = (ranksDescending zip suits).map(t => Card(t._1, t._2)).toSet

    HighCard(cards, ranksDescending)
  }
}