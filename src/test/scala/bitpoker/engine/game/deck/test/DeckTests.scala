package bitpoker.engine.game.deck.test


import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import bitpoker.engine.game.deck.Deck
import bitpoker.engine.handranking.test.TestDecks

class DeckSpec extends FunSpec with BeforeAndAfter with ShouldMatchers {

  val cardSequence = TestDecks.shuffledDeck1
  var deck: Deck = _

  before {
    deck = new Deck(cardSequence)
  }

  describe("A deck") {
    it("should remove dealt cards from the remaining pile") {
      val numCardsToDeal = 5
      val initialSize = deck.remaining.size
      deck.deal(numCardsToDeal)
      val resultingSize = deck.remaining.size

      resultingSize should equal(initialSize - numCardsToDeal)
    }

    it("should deal cards out in order") {
      for (card <- cardSequence) {
        Seq(card) should equal(deck.deal(1))
      }
    }

    it("should deal the remaining cards if asked to deal more than the remaining cards") {
      val startingSize = deck.remaining.size
      val numCardsToDeal = startingSize + 100
      val dealt = deck.deal(numCardsToDeal)

      dealt.size should equal(startingSize)
      deck.remaining.size should equal(0)
    }
  }

  describe("An empty deck") {
    it("should deal an empty sequence when asked to deal any cards") {
      deck = new Deck(Seq.empty)
      val dealt = deck.deal(100)
      dealt should equal(Seq.empty)
    }

  }
}