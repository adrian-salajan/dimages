package dimages


object Transform {

  def rotateClockwise(angle: Double): Loc => Loc = {
    case Loc(x, y) =>
      import Math._
      Loc(
        cos(angle)*x + (-sin(angle)*y),
        sin(angle)*x + cos(angle) * y,
      )
  }

}
