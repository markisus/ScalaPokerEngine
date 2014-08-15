package bitpoker.engine.game.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import bitpoker.engine.game.BetComputations._
import bitpoker.engine.game.GameComputation.chain
import bitpoker.engine.game.GameComputation.noop
import bitpoker.engine.game.GameKernel
import bitpoker.engine.game.JoinComputations._
import bitpoker.engine.game.StartComputations.start
import bitpoker.engine.game.FoldComputations.fold
import bitpoker.engine.game.UtilityComputations._
import bitpoker.engine.handranking.test.TestDecks

class UserOrderSepc extends FunSpec with BeforeAndAfter with ShouldMatchers {

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

  describe("when all players have moved") {
    it("should advance the next dealer correctly and active user positions and big blind and small blind") {
      game = fold(1).exec(game)
      game = fold(2).exec(game)
      game = start(1).exec(game)
      game.dealerPosition should equal(5)
      game.activeUserPosition should equal(5)
      game.bigBlindPosition should equal(1)
      game.smallBlindPosition should equal(6)
    }
  }

}