package bitpoker.engine.game

object Utility {
  def isInRange(number: Long, low: Long, high: Long): Boolean =
    (number >= low) && (number <= high)

  def symmetricDiff[A](a: Set[A], b: Set[A]): Set[A] =
    a.diff(b) union b.diff(a)

  def findKeysByValues[A, B](map: Map[A, B], f: B => Boolean): Set[A] =
    map.filter(kv => f(kv._2)).map(kv => kv._1).toSet

  def flipMap[A, B](map: Map[A, B]): Map[B, A] =
    map.map({ case (a, b) => (b, a) }).toMap

  def nextGreatest(
    currentNumber: Int,
    numbers: Set[Int],
    default: Int = 0): Int = {

    lazy val greater = numbers.filter(_ > currentNumber)
    lazy val lesser = numbers.filter(_ < currentNumber)
    val answer = if (greater.isEmpty)
      if (lesser.isEmpty)
        default
      else
        lesser.min
    else
      greater.min

    answer
  }
}