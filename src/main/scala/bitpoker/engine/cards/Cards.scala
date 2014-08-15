package bitpoker.engine.cards

object Rank extends Enumeration {
  type Rank = Value
  val Two = Value(2, "2")
  val Three = Value(3, "3")
  val Four = Value(4, "4")
  val Five = Value(5, "5")
  val Six = Value(6, "6")
  val Seven = Value(7, "7")
  val Eight = Value(8, "8")
  val Nine = Value(9, "9")
  val Ten = Value(10, "10")
  val Jack = Value(11, "Jack")
  val Queen = Value(12, "Queen")
  val King = Value(13, "King")
  val Ace = Value(14, "Ace")

  def next(rank: Rank) = rank match {
    case Ace => Two
    case default => Rank(default.id + 1)
  }

  def previous(rank: Rank) = rank match {
    case Two => Ace
    case default => Rank(default.id - 1)
  }

  def previous(rank: Rank, times: Int): Rank = {
    if (times <= 0) rank
    else previous(previous(rank), times - 1)
  }
}

case class Card(rank: Rank.Value, suit: Suit.Value) {
  override def toString() = {
    rank + " of " + suit;
  }
}