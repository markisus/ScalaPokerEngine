package bitpoker.engine.handranking

import bitpoker.engine.cards.Rank
import bitpoker.engine.cards.Card

trait HandRefiner {
  def best5(hand: Hand7): Hand5
}

object NaiveHandRefiner extends HandRefiner {
  def best5(hand: Hand7): Hand5 = {
    implicit def func[A](item: => A) = () => item

    def search(
      default: () => Hand5,
      options: () => Option[Hand5]*): Hand5 =
      options.view.flatMap(option => option()).headOption.getOrElse(default())

    val mostValuableSingles5 = mostValuableSingles(hand.cards, 5)

    search(
      HighCard(mostValuableSingles5, mostValuableSingles5.map(_.rank).toSeq.sorted.reverse),

      findSuperFlush(hand.cards).map(cards => {
        //When we have a super-flush, then our best hand will be a flush
        val straightFlush = findStraight(cards)
        straightFlush match {
          //If the straight-flush exists, it may be a royal flush
          case Some(sf) if (sf.highRank == Rank.Ace) => RoyalFlush(cards)
          //Or it may be a normal straight-flush
          case Some(sf) => StraightFlush(sf.cards, sf.highRank)
          //If the straight-flush does not exist, then we have a plain flush
          case None => findFlush(hand).get
        }
      }),
      findQuads(hand),
      findFullHouse(hand),
      findStraight(hand.cards),
      findThreeOfAKind(hand),
      findTwoPair(hand),
      findPair(hand))
  }

  private[this] def makeContiguousRanks(highRank: Rank.Value, size: Int): Set[Rank.Value] = {
    if (size == 0) Set()
    else makeContiguousRanks(Rank.previous(highRank), size - 1) + highRank
  }

  private[this] def findStraight(hand: Set[Card]): Option[Straight] = {
    //We cannot make the input a Hand7 because we may be passed in 6 cards
    //when searching for a straight-flush, so expose any stupid programmer errors
    require(hand.size >= 5)

    val ranks = hand.map(_.rank)
    val upperDiscontinuities = ranks.filter(rank => !ranks(Rank.next(rank)))
    val possibleStraightHighs = upperDiscontinuities.filter(_ >= Rank.Five)
    val possibleStraights = possibleStraightHighs.map(makeContiguousRanks(_, 5))
    val straights = possibleStraights.filter(_.subsetOf(ranks))

    //If the hand contains one of the possibleStraights, it must be unique
    //or else the hand had 10 elements (5*2), but we know hands can have only 7
    val straightRanks = straights.headOption

    //Turn the set of ranks back into a set of cards
    val cardsByRank = hand.groupBy(_.rank)

    val highRank = straightRanks.map(ranks => {
      //Take care of the special case where we have an A2345 straight,
      //for which the high card is not the max ranked Ace
      if (ranks.contains(Rank.Ace) && ranks.contains(Rank.Five)) Rank.Five
      else ranks.max
    })

    val straightCards = straightRanks.map(ranks => ranks.map(cardsByRank(_).head))
    (straightCards, highRank) match {
      case (Some(straight), Some(rank)) => Some(Straight(straight, rank))
      case _ => None
    }
  }

  //If a hand contains a flush of one suit, it cannot have a flush of another suit
  //therefore we are getting the best flush hand by taking headOption
  private[this] def findSuperFlush(hand: Set[Card]): Option[Set[Card]] =
    hand.groupBy(_.suit).values.filter(_.size >= 5).headOption

  private[this] def findFlush(hand: Hand7): Option[Flush] = {
    val superFlush = findSuperFlush(hand.cards)
    val flushCards = superFlush.map(
      flush => flush.toSeq.sortBy(_.rank).take(5).toSet)
    val ranksDescending = flushCards.map(some => some.map(_.rank).toSeq.sorted.reverse)

    (flushCards, ranksDescending) match {
      case (Some(flush), Some(ranks)) => Some(Flush(flush, ranks))
      case _ => None
    }
  }

  private[this] def findQuads(hand: Hand7): Option[Quads] = {
    val quadCards = mostValuableMultiples(hand.cards, 4)
    quadCards.map(quads => {
      val kicker = mostValuableSingles(hand.cards.diff(quads), 1).head
      Quads(quads + kicker, quads.map(_.rank).head, kicker.rank)
    })
  }

  private[this] def mostValuableMultiples(hand: Set[Card], times: Int): Option[Set[Card]] = {
    val multiples = hand.groupBy(_.rank).filter(_._2.size >= times).toSeq.sortBy(_._1).map(_._2)
    multiples.lastOption.map(cards => cards.take(times))
  }

  private[this] def mostValuableSingles(hand: Set[Card], number: Int): Set[Card] = {
    if (number < 0) Set()
    else hand.toSeq.sortBy(_.rank).takeRight(number).toSet
  }

  private[this] def findFullHouse(hand: Hand7): Option[FullHouse] = {
    val trips = mostValuableMultiples(hand.cards, 3)
    val doubles = trips match {
      case Some(cards) => mostValuableMultiples(hand.cards.diff(cards), 2)
      case None => None
    }

    (trips, doubles) match {
      case (Some(a), Some(b)) => Some(FullHouse(a ++ b, a.head.rank, b.head.rank))
      case _ => None
    }
  }

  private[this] def findTwoPair(hand: Hand7): Option[TwoPair] = {
    val firstPair = mostValuableMultiples(hand.cards, 2)
    val secondPair = firstPair match {
      case Some(cards) => mostValuableMultiples(hand.cards.diff(cards), 2)
      case None => None
    }

    (firstPair, secondPair) match {
      case (Some(a), Some(b)) => {
        val twoPairCards = a ++ b
        val twoPairRanks = twoPairCards.map(_.rank).toSeq.sorted
        val highRank = twoPairRanks(1)
        val lowRank = twoPairRanks(0)
        val kicker = mostValuableSingles(hand.cards.diff(twoPairCards), 1).head
        Some(TwoPair(twoPairCards + kicker, highRank, lowRank, kicker.rank))
      }
      case _ => None
    }
  }

  private[this] def findThreeOfAKind(hand: Hand7): Option[ThreeOfAKind] = {
    val trips = mostValuableMultiples(hand.cards, 3)
    trips map { tripCards =>
      {
        val otherCards = mostValuableSingles(hand.cards.diff(tripCards), 2)
        ThreeOfAKind(
            tripCards ++ otherCards, 
            tripCards.head.rank, 
            otherCards.map(_.rank).max)
      }
    }
  }

  private[this] def findPair(hand: Hand7): Option[OnePair] = {
    val pair = mostValuableMultiples(hand.cards, 2)
    pair map { pairCards =>
      {
        val otherCards = mostValuableSingles(hand.cards.diff(pairCards), 3)
        OnePair(pairCards ++ otherCards,
            pairCards.head.rank, 
            otherCards.map(_.rank).toSeq.sorted.reverse)
      }
    }
  }
}