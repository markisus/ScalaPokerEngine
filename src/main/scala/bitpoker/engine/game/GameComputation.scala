package bitpoker.engine.game
//Class for sequencing alterations to game kernels

class GameComputation[+R](val computation: GameKernel => (GameKernel, R)) {

  def exec(gameKernel: GameKernel): GameKernel =
    computation(gameKernel)._1

  def eval(gameKernel: GameKernel): R =
    computation(gameKernel)._2

  //Convert the result of a computation into something else
  def map[B](f: R => B): GameComputation[B] = {
    new GameComputation(
      gameKernel => {
        val (newGameKernel, computationResult) = computation(gameKernel)
        (newGameKernel, f(computationResult))
      })
  }

  //Sequence two computations together
  def flatMap[B](f: R => GameComputation[B]): GameComputation[B] = {
    new GameComputation(
      gameKernel => {
        lazy val (newGameKernel, computationResult) = computation(gameKernel)
        f(computationResult).computation(newGameKernel)
      })
  }

  //Sequence this computation with another one, throwing away the output of this one
  def andThen[B](gameComputation: GameComputation[B]): GameComputation[B] = {
    this.flatMap(_ => gameComputation)
  }
}

object GameComputation {

  def apply[R](computation: GameKernel => (GameKernel, R)) =
    new GameComputation(computation)

  //A no-op computation that just returns the value
  def emit[B](value: B): GameComputation[B] =
    GameComputation(gameKernel => (gameKernel, value))

  val noop: GameComputation[Unit] = emit(())

  //Compute some value from the game kernel without changing it
  def get[B](getValue: GameKernel => B) =
    GameComputation(gameKernel => (gameKernel, getValue(gameKernel)))

  //put a new game kernel, ignore results
  def put(updater: GameKernel => GameKernel): GameComputation[Unit] =
    GameComputation(gameKernel => (updater(gameKernel), ()))

  //Implicit conversion
  implicit def functionToComputation[R](
    computation: GameKernel => (GameKernel, R)): GameComputation[R] =
    GameComputation(computation)

  //Do a bunch of computations in sequence
  def chain(gameComputations: Iterable[GameComputation[_]]): GameComputation[_] =
    gameComputations.foldLeft(noop)((gc1, gc2) => gc1 andThen gc2.map(_=>()))

  private def append[R](gc1: GameComputation[Seq[R]], gc2: GameComputation[R]) =
    for {
      seqR <- gc1
      oneR <- gc2
    } yield seqR :+ oneR

  private def extend[R](gc1: GameComputation[Seq[R]], gc2: GameComputation[Seq[R]]) =
    for {
      seqR <- gc1
      oneR <- gc2
    } yield seqR ++ oneR

  def multi[R](gameComputations: Seq[GameComputation[R]]): GameComputation[Seq[R]] =
    gameComputations.foldLeft(emit[Seq[R]](Seq.empty))(
      (gc1, gc2) => append(gc1, gc2))

  def filter[R](
    items: Seq[R],
    predComputation: R => GameComputation[Boolean]): GameComputation[Seq[R]] = {

    val itemToSeq: R => GameComputation[Seq[R]] =
      a => predComputation(a).map(b =>
        if (b) Seq(a) else Seq.empty)

    items.foldLeft(emit[Seq[R]](Seq.empty))(
      (gc, i) => extend(gc, itemToSeq(i)))
  }
}