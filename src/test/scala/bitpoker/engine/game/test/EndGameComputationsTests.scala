package bitpoker.engine.game.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import bitpoker.engine.game.BetComputations._
import bitpoker.engine.game.GameComputation.chain
import bitpoker.engine.game.GameKernel
import bitpoker.engine.game.JoinComputations._
import bitpoker.engine.game.StartComputations.start
import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.handranking.test.TestDecks
import bitpoker.engine.game.WonShowdown
import bitpoker.engine.handranking.ThreeOfAKind
import bitpoker.engine.cards.Card
import bitpoker.engine.cards.Rank
import bitpoker.engine.cards.Suit
import bitpoker.engine.handranking.OnePair

class EndGameComputaionsSpec extends FunSpec with BeforeAndAfter with ShouldMatchers {

  val testGameKernel = TestGameKernel.gameKernel
  var game: GameKernel = testGameKernel

  before {
    val joinComputations = (for {
      _ <- join(1, 1, 100, "Player 1")
      _ <- join(2, 5, 200, "Player 2")
      _ <- join(3, 6, 300, "Player 3")
      _ <- start(1, () => TestDecks.shuffledDeck1)
    } yield ())

    game = joinComputations.exec(game)
  }
  
  describe("when players move") {
    it("should advance the next player correctly") {
      
    }
    
  }

  describe("when everyone goes all in") {
    it("should calculate winners side pots correctly") {
      def allInComputation(userid: Int) = for {
        maybeUserMoney <- getUserMoney(userid)
        _ <- bet(userid, maybeUserMoney.getOrElse(0))
      } yield ()

      val allInComputations = chain(Seq(1, 2, 3).map(allInComputation))

      val gameResults = allInComputations.andThen(getGameResults).eval(game)

      gameResults.results should contain(
        WonShowdown(2, 300, 300, ThreeOfAKind(Set(
          Card(Rank.Seven, Suit.Clubs),
          Card(Rank.Seven, Suit.Spades),
          Card(Rank.Queen, Suit.Spades),
          Card(Rank.Ten, Suit.Diamonds),
          Card(Rank.Seven, Suit.Diamonds)), Rank.Seven, Rank.Queen)))

      gameResults.results should contain(
        WonShowdown(2, 200, 200, ThreeOfAKind(Set(
          Card(Rank.Seven, Suit.Clubs),
          Card(Rank.Seven, Suit.Spades),
          Card(Rank.Queen, Suit.Spades),
          Card(Rank.Ten, Suit.Diamonds),
          Card(Rank.Seven, Suit.Diamonds)), Rank.Seven, Rank.Queen)))

      gameResults.results should contain(
        WonShowdown(3, 100, 100, OnePair(Set(
          Card(Rank.Seven, Suit.Spades),
          Card(Rank.Jack, Suit.Hearts),
          Card(Rank.Queen, Suit.Spades),
          Card(Rank.Ten, Suit.Diamonds),
          Card(Rank.Seven, Suit.Diamonds)), Rank.Seven, Seq(Rank.Queen, Rank.Jack, Rank.Ten))))
    }
    
    
  }

}