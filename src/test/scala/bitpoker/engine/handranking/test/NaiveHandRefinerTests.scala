package bitpoker.engine.handranking.test;
import org.scalatest._
import scala.util.Sorting
import bitpoker.engine._
import org.scalatest.matchers.ShouldMatchers._
import bitpoker.engine.handranking.NaiveHandRefiner
import bitpoker.engine.handranking.TwoPair
import bitpoker.engine.handranking.HighCard
import bitpoker.engine.handranking.Flush
import bitpoker.engine.handranking.StraightFlush
import bitpoker.engine.handranking.Quads
import bitpoker.engine.handranking.OnePair
import bitpoker.engine.handranking.ThreeOfAKind
import bitpoker.engine.handranking.Straight
import bitpoker.engine.handranking.RoyalFlush

class NaiveHandRefinerSpec extends FunSpec {
  describe("A NaiveHandRefiner") {
    it("should properly identify a Royal Flush") {
      val royalFlush = NaiveHandRefiner.best5(TestHand7s.royalFlush7a)
      val isRoyalFlush = royalFlush match {
        case RoyalFlush(_) => true
        case _ => false
      }
      assert(isRoyalFlush)
    }
    
    it("should properly identify a Straight Flush") {
      val straightFlush = NaiveHandRefiner.best5(TestHand7s.straightFlush7)
      val isStraightFlush = straightFlush match {
        case StraightFlush(_,_) => true
        case _ => false
      }
    }
    
    it("should properly identify Quads") {
      val quads = NaiveHandRefiner.best5(TestHand7s.quad7)
      val isQuads = quads match {
        case Quads(_,_,_) => true
        case _ => false
      }
    }
    
    it("should properly identify a Flush") {
      val flush = NaiveHandRefiner.best5(TestHand7s.flush7)
      val isFlush = flush match {
        case Flush(_,_) => true
        case _ => false
      } 
    }
    
    it("should properly identify a Straight") {
      val straight = NaiveHandRefiner.best5(TestHand7s.straight7)
      val isStraight = straight match {
        case Straight(_,_) => true
        case _ => false
      }
    }
    
    it("should properly identify a Three of a Kind") {
      val trips = NaiveHandRefiner.best5(TestHand7s.trips7)
      val isTrips = trips match {
        case ThreeOfAKind(_,_,_) => true
        case _ => false
      }
    }
    
    it("should properly identify Two Pair") {
      val twoPair = NaiveHandRefiner.best5(TestHand7s.twoPair7)
      val isTwoPair = twoPair match {
        case TwoPair(_,_,_,_) => true
        case _ => false
      }
    }
    
    it("should properly identify a Pair") {
      val onePair = NaiveHandRefiner.best5(TestHand7s.pair7)
      val isOnePair = onePair match {
        case OnePair(_,_,_) => true
        case _ => false
      }
    }
    
    it("should properly identify a High Card") {
      val highCard = NaiveHandRefiner.best5(TestHand7s.high7)
      val isHighCard = highCard match {
        case HighCard(_,_) => true
        case _ => false
      }
    }
  }
}