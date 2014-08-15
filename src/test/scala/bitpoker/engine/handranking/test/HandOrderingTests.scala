package bitpoker.engine.handranking.test

import org.scalatest._
import scala.util.Sorting
import org.scalatest.matchers.ShouldMatchers._
import bitpoker.engine.cards._
import bitpoker.engine.handranking.HandOrdering
import bitpoker.engine.handranking.Hand5
import bitpoker.engine.cards.Rank

class HandOrderingSpec extends FunSpec {
  def assertHandOrderingMatches(correctOrdering: Seq[Hand5]) {
    assert(correctOrdering.sorted(HandOrdering) == correctOrdering)
  }

  def assertAllTied(hands: Seq[Hand5]) {
    val comparisons = hands.combinations(2)
      .map(twoHands => HandOrdering.compare(twoHands(0), twoHands(1)))

    assert(comparisons.forall(_ == 0))
  }

  def assertReflexive(hand: Hand5) {
    assert(HandOrdering.compare(hand, hand) == 0)
  }

  describe("A HandOrdering") {
    it("should order hands in accordance to the rules of poker") {
      val correctOrder = Seq(
        TestHand5s.makeHighCard(Seq(Rank.Ace, Rank.Six, Rank.Four, Rank.Three, Rank.Two)),
        TestHand5s.makeOnePair(Rank.Ace, Seq(Rank.Five, Rank.Four, Rank.Two)),
        TestHand5s.makeTwoPair(Rank.Ace, Rank.King, Rank.Queen),
        TestHand5s.makeThreeOfAKind(Rank.Ace, Rank.King),
        TestHand5s.makeStraight(Rank.Ace),
        TestHand5s.makeFlush(Seq(Rank.Ten, Rank.Seven, Rank.Five, Rank.Three, Rank.Two)),
        TestHand5s.makeFullHouse(Rank.Ace, Rank.King),
        TestHand5s.makeQuads(Rank.Ace, Rank.King),
        TestHand5s.makeRoyalFlush())
      assertHandOrderingMatches(correctOrder)
    }

    it("should tie any Royal Flushes of different suit are equal") {
      val royalFlushes = Seq(
        Suit.Clubs,
        Suit.Hearts,
        Suit.Diamonds,
        Suit.Spades)
        .map(TestHand5s.makeRoyalFlush(_))
      assertAllTied(royalFlushes)
    }

    it("should tie any Straight Flushes with the same high rank are equal") {
      val highCards = Rank.values.toSeq.sorted.filter(_ >= Rank.Five)
      highCards.foreach(highCard => {
        val straightFlushes = Suit.values.map(TestHand5s.makeStraightFlush(highCard, _))
        assertAllTied(straightFlushes.toSeq)
      })
    }

    it("should properly order any Straight Flushes with different high rank") {
      val highCards = Rank.values.toSeq.sorted.filter(_ >= Rank.Five)
      val orderedStraightFlushes = highCards.map(TestHand5s.makeStraightFlush(_))
      assertHandOrderingMatches(orderedStraightFlushes)
    }

    it("should properly order between same Quads with different high rank") {
      val quadRank = Rank.Two
      val otherRanks = (Rank.values.toSet - quadRank).toSeq.sorted

      val orderedQuads = otherRanks.toSeq.sorted.map(
        TestHand5s.makeQuads(quadRank, _))
      assertHandOrderingMatches(orderedQuads)
    }

    it("should say that Quads with the same four-rank and the same high rank are equal") {
      val quads = Rank.values.toSeq.combinations(2)
        .map(twoRanks => TestHand5s.makeQuads(twoRanks(0), twoRanks(1)))
      quads.foreach(assertReflexive(_))
    }

    it("should properly order different Quads with the same high card") {
      val quadRanks = (Rank.values.toSet - Rank.Ace).toSeq.sorted
      val orderedQuads = quadRanks.map(TestHand5s.makeQuads(_, Rank.Ace))
      assertHandOrderingMatches(orderedQuads)
    }

    it("should properly order different FullHouses with the same triple but different doubles") {
      val tripleRank = Rank.Two
      val doubleRanks = (Rank.values - tripleRank).toSeq.sorted
      val orderedFullHouses = doubleRanks.map(TestHand5s.makeFullHouse(tripleRank, _))
      assertHandOrderingMatches(orderedFullHouses)
    }

    it("should properly order different FullHouses with different triples but the same double") {
      val doubleRank = Rank.Ace
      val tripleRanks = (Rank.values - doubleRank).toSeq.sorted
      val orderedFullHouses = tripleRanks.map(TestHand5s.makeFullHouse(_, doubleRank))
      assertHandOrderingMatches(orderedFullHouses)
    }

    it("should say that FullHouses with the same double and triple are equal") {
      val fullHouses = Rank.values.toSeq.combinations(2).map(
        twoRanks => TestHand5s.makeFullHouse(twoRanks(0), twoRanks(1)))
      fullHouses.foreach(assertReflexive(_))
    }

    it("should properly order two Flushes that differ in the highest rank") {
      val sameRanks = Seq(Rank.Five, Rank.Four, Rank.Three, Rank.Two)
      val diffRanks = Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King)
      val orderedFlushes = diffRanks.map(rank => TestHand5s.makeFlush(rank +: sameRanks))
      assertHandOrderingMatches(orderedFlushes)
    }

    it("should properly order two Flushes that differ in the second highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.Four, Rank.Three, Rank.Two)
      val diffRanks = Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King)
      val orderedFlushes = diffRanks.map(
        rank => TestHand5s.makeFlush((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedFlushes)
    }

    it("should properly order two Flushes that differ in the third highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.King, Rank.Three, Rank.Two)
      val diffRanks = Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack)
      val orderedFlushes = diffRanks.map(
        rank => TestHand5s.makeFlush((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedFlushes)
    }

    it("should properly order two Flushes that differ in the fourth highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.King, Rank.Queen, Rank.Two)
      val diffRanks = Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack)
      val orderedFlushes = diffRanks.map(
        rank => TestHand5s.makeFlush((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedFlushes)
    }

    it("should properly order two Flushes that differ in the last highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.King, Rank.Queen, Rank.Jack)
      val diffRanks = Seq(Rank.Two, Rank.Three, Rank.Four, Rank.Six)
      val orderedFlushes = diffRanks.map(
        rank => TestHand5s.makeFlush((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedFlushes)
    }

    it("should tie Flushes with equal ranks") {
      val flush = TestHand5s.makeFlush(Seq(Rank.Ace, Rank.King, Rank.Queen, Rank.Jack, Rank.Nine))
      assert(HandOrdering.compare(flush, flush) == 0)
    }

    it("should tie any Straights with the same high rank") {
      val highCards = Rank.values.toSeq.sorted.filter(_ >= Rank.Five)
      val straights = highCards.map(TestHand5s.makeStraight(_))
      straights.foreach(assertReflexive(_))
    }

    it("should properly order any Straights with different high rank") {
      val highCards = Rank.values.toSeq.sorted.filter(_ >= Rank.Five)
      val orderedStraights = highCards.map(TestHand5s.makeStraight(_))
      assertHandOrderingMatches(orderedStraights)
    }

    it("should properly order any Three of a Kind with same triples but different kicker rank") {
      val tripRank = Rank.Two
      val kickerRanks = (Rank.values.toSet - tripRank).toSeq.sorted
      val orderedTrips = kickerRanks.map(
        kickerRank => TestHand5s.makeThreeOfAKind(tripRank, kickerRank))
      assertHandOrderingMatches(orderedTrips)
    }

    it("should tie Three of a Kinds with same triples and same kickers") {
      val threeOfAKind = TestHand5s.makeThreeOfAKind(Rank.Ace, Rank.Two)
      assertReflexive(threeOfAKind)
    }

    it("should properly order any Two-Pair with different high pair ranks") {
      val lowPairRank = Rank.Two
      val kickerRank = Rank.Ace
      val ranks = (Rank.values.toSet - kickerRank).filter(_ > lowPairRank).toSeq.sorted
      val orderedTwoPairs = ranks.map(TestHand5s.makeTwoPair(_, lowPairRank, kickerRank))
      assertHandOrderingMatches(orderedTwoPairs)
    }

    it("should properly order any Two-Pair with different low pair ranks") {
      val highPairRank = Rank.Ace
      val kickerRank = Rank.King
      val ranks = (Rank.values.toSet - kickerRank).filter(_ < highPairRank).toSeq.sorted
      val orderedTwoPairs = ranks.map(TestHand5s.makeTwoPair(highPairRank, _, kickerRank))
      assertHandOrderingMatches(orderedTwoPairs)
    }

    it("should properly order Two-Pairs with different kickers") {
      val highPairRank = Rank.Ace
      val lowPairRank = Rank.King
      val ranks = (Rank.values.toSet - highPairRank - lowPairRank).toSeq.sorted
      val orderedTwoPairs = ranks.map(TestHand5s.makeTwoPair(highPairRank, lowPairRank, _))
      assertHandOrderingMatches(orderedTwoPairs)
    }

    it("should tie Two-Pairs with the same ranks") {
      val twoPair = TestHand5s.makeTwoPair(Rank.Three, Rank.Two, Rank.Ace)
      assertReflexive(twoPair)
    }

    it("should properly order Pairs by the pair rank") {
      val highRank = Rank.Ace
      val secondHigh = Rank.Jack
      val thirdHigh = Rank.Ten
      val pairRanks = (Rank.values.toSet - highRank - secondHigh - thirdHigh).toSeq.sorted
      val orderedPairs = pairRanks.map(
        TestHand5s.makeOnePair(_, Seq(highRank, secondHigh, thirdHigh)))
      assertHandOrderingMatches(orderedPairs)
    }

    it("should properly order Pairs by the high rank when pairs when otherwise tied") {
      val secondHigh = Rank.Three
      val thirdHigh = Rank.Two
      val pairRank = Rank.Six
      val highRanks = (Rank.values.toSet - pairRank - secondHigh - thirdHigh)
        .filter(_ > secondHigh).toSeq.sorted
      val orderedPairs = highRanks.map(
        highRank => TestHand5s.makeOnePair(pairRank, Seq(highRank, secondHigh, thirdHigh)))
      assertHandOrderingMatches(orderedPairs)
    }

    it("should properly order Pairs by the second high rank when otherwise tied") {
      val highRank = Rank.Ace
      val thirdHigh = Rank.Two
      val pairRank = Rank.Six
      val secondHighRanks = (Rank.values.toSet - pairRank - highRank - thirdHigh)
        .filter(_ > thirdHigh).filter(_ < highRank).toSeq.sorted
      val orderedPairs = secondHighRanks.map(
        secondRank => TestHand5s.makeOnePair(pairRank, Seq(highRank, secondRank, thirdHigh)))
      assertHandOrderingMatches(orderedPairs)
    }

    it("should properly order Pairs by the third high rank when otherwise tied") {
      val highRank = Rank.Ace
      val secondHigh = Rank.Jack
      val pairRank = Rank.Six
      val thirdHighRanks = (Rank.values.toSet - pairRank - highRank - secondHigh)
        .filter(_ < secondHigh).toSeq.sorted
      val orderedPairs = thirdHighRanks.map(
        thirdHigh => TestHand5s.makeOnePair(pairRank, Seq(highRank, secondHigh, thirdHigh)))
      assertHandOrderingMatches(orderedPairs)
    }

    it("should tie Pairs with the same ranks") {
      val pair = TestHand5s.makeOnePair(Rank.Ace, Seq(Rank.Nine, Rank.Eight, Rank.Five))
      assertReflexive(pair)
    }

    it("should properly order two HighCard hands that differ in the highest rank") {
      val sameRanks = Seq(Rank.Five, Rank.Four, Rank.Three, Rank.Two)
      val diffRanks = Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King)
      val orderedHands = diffRanks.map(rank => TestHand5s.makeHighCard(rank +: sameRanks))
      assertHandOrderingMatches(orderedHands)
    }

    it("should properly order two HighCard that differ in the second highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.Four, Rank.Three, Rank.Two)
      val diffRanks = Seq(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King)
      val orderedHands = diffRanks.map(
        rank => TestHand5s.makeHighCard((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedHands)
    }

    it("should properly order two HighCard that differ in the third highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.King, Rank.Three, Rank.Two)
      val diffRanks = Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack)
      val orderedFlushes = diffRanks.map(
        rank => TestHand5s.makeHighCard((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedFlushes)
    }

    it("should properly order two HighCard that differ in the fourth highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.King, Rank.Queen, Rank.Two)
      val diffRanks = Seq(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack)
      val orderedHands = diffRanks.map(
        rank => TestHand5s.makeHighCard((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedHands)
    }

    it("should properly order two HighCard that differ in the last highest rank") {
      val sameRanks = Seq(Rank.Ace, Rank.King, Rank.Queen, Rank.Jack)
      val diffRanks = Seq(Rank.Two, Rank.Three, Rank.Four, Rank.Six)
      val orderedHands = diffRanks.map(
        rank => TestHand5s.makeHighCard((rank +: sameRanks).sorted.reverse))
      assertHandOrderingMatches(orderedHands)
    }
  }
}