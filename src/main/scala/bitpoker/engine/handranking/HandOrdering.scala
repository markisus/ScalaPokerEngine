package bitpoker.engine.handranking
import bitpoker.engine.cards.Rank

object HandOrdering extends Ordering[Hand5] {
  private[this] def handScore(hand: Hand5): Int = hand match {
    case RoyalFlush(_) => 10
    case StraightFlush(_, _) => 9
    case Quads(_, _, _) => 8
    case FullHouse(_, _, _) => 7
    case Flush(_, _) => 6
    case Straight(_, _) => 5
    case ThreeOfAKind(_, _, _) => 4
    case TwoPair(_, _, _, _) => 3
    case OnePair(_, _, _) => 2
    case HighCard(_, _) => 1
  }

  private[this] def rankCompare(a: Rank.Value, b: Rank.Value) = a.id - b.id

  //Find the first index at which the sequences differ and use the difference
  //as the comparison value
  private[this] def hastySequenceCompare(aRanks: Seq[Rank.Value], bRanks: Seq[Rank.Value]) =
    (aRanks zip bRanks).map(t => rankCompare(t._1, t._2)).find(_ != 0).getOrElse(0)

  private[this] def tieBreaker(aHand5: Hand5, bHand5: Hand5): Int = {
    (aHand5, bHand5) match {
      case (RoyalFlush(_), RoyalFlush(_)) => 0
      case (Quads(_, aQuadRank, aKickerRank), Quads(_, bQuadRank, bKickerRank)) => {
        if (aQuadRank != bQuadRank)
          rankCompare(aQuadRank, bQuadRank)
        else
          rankCompare(aKickerRank, bKickerRank)
      }
      case (StraightFlush(_, aHigh), StraightFlush(_, bHigh)) => rankCompare(aHigh, bHigh)
      case (FullHouse(_, aTrips, aDubs), FullHouse(_, bTrips, bDubs)) => {
        if (aTrips != bTrips)
          rankCompare(aTrips, bTrips)
        else
          rankCompare(aDubs, bDubs)
      }
      case (Flush(_, aRanks), Flush(_, bRanks)) => {
        hastySequenceCompare(aRanks, bRanks)
      }
      case (Straight(_, aHigh), Straight(_, bHigh)) => aHigh.id - bHigh.id
      case (ThreeOfAKind(_, aTripRank, aKicker), ThreeOfAKind(_, bTripRank, bKicker)) => {
        if (aTripRank != bTripRank)
          rankCompare(aTripRank, bTripRank)
        else
          rankCompare(aKicker, bKicker)
      }
      case (TwoPair(_, aHigh, aLow, aKicker), TwoPair(_, bHigh, bLow, bKicker)) => {
        if (aHigh != bHigh) rankCompare(aHigh, bHigh)
        else (if (aLow != bLow) rankCompare(aLow, bLow)
        else rankCompare(aKicker, bKicker))
      }
      case (OnePair(_, aRank, aRest), OnePair(_, bRank, bRest)) => {
        if (aRank != bRank) rankCompare(aRank, bRank)
        else hastySequenceCompare(aRest, bRest)
      }
      case (HighCard(_, aRanks), HighCard(_, bRanks)) => hastySequenceCompare(aRanks, bRanks)
    }
  }

  override def compare(aHand5: Hand5, bHand5: Hand5): Int = {
    val aScore = handScore(aHand5)
    val bScore = handScore(bHand5)
    if (handScore(aHand5) == handScore(bHand5))
      tieBreaker(aHand5, bHand5)
    else
      aScore - bScore
  }
}