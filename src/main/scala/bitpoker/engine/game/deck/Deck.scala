package bitpoker.engine.game.deck
import bitpoker.engine.cards.Card
import bitpoker.engine.cards.DeckMaker

class Deck(initial: Seq[Card]) {
  private[deck] var remaining = initial

  def deal(numToDeal: Int): Seq[Card] = {
    val dealt = remaining.take(numToDeal)
    remaining = remaining.drop(numToDeal)
    return dealt
  }
}

object Deck {
  def apply(): Deck = new Deck(DeckMaker.makeShuffledDeck)
}