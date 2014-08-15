package bitpoker.engine.game.denominations

trait Denomination {
  def asInt: Int
}

case object x1 extends Denomination {
  def asInt = 1
}
case object x100 extends Denomination {
  def asInt = 100
}
case object x100000 extends Denomination {
  def asInt = 100000
}
case object x100000000 extends Denomination {
  def asInt = 100000000
}

object Denomination {
  def fromString(d: String): Denomination = {
    d match {
      case "x1" => x1
      case "x100" => x100
      case "x100000" => x100000
      case "x100000000" => x100000000
      case e => throw new Exception("Did not understand denomination " + e)
    }
  }
}