package bitpoker.engine.game

trait GameState

case object WaitForPlayers extends GameState
case object PreFlop extends GameState
case object Flop extends GameState
case object Turn extends GameState
case object River extends GameState

