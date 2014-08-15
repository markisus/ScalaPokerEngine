package bitpoker.engine.cards

object Suit extends Enumeration {
  type Suit = Value
  val Diamonds = Value(0, "Diamonds")
  val Clubs = Value(1, "Clubs")
  val Hearts = Value(2, "Hearts")
  val Spades = Value(3, "Spades")
}