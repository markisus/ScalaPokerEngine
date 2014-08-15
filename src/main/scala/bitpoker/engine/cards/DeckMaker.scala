package bitpoker.engine.cards

import scala.util.Random.shuffle

object DeckMaker {
  def makeSomeDeck: Seq[Card] =
    (for (rank <- Rank.values; suit <- Suit.values) yield Card(rank, suit)).toSeq
  def makeOrderedDeck: Seq[Card] =
    (for (rank <- Rank.values.toSeq.sorted; suit <- Suit.values.toSeq.sorted) yield Card(rank, suit)).toSeq
  def makeShuffledDeck: Seq[Card] =
    shuffle(makeSomeDeck)
}
