package bitpoker.engine.game

trait PlayerState

case object JustJoined extends PlayerState
case object ForcingBigBlind extends PlayerState
case object Folded extends PlayerState
case object Ready extends PlayerState
case object AllIn extends PlayerState
case object Busted extends PlayerState