package dimages

import cats.kernel.Monoid


case class Color(r: Float, g: Float, b: Float, a: Float) {
  val brightness: Float = (r + g + b) / 3

  val toInt: Int = (((a * 255).toInt & 0xFF) << 24) |
    (((r * 255).toInt & 0xFF) << 16) |
    (((g * 255).toInt & 0xFF) << 8) |
    (((b * 255).toInt & 0xFF) << 0)

}


object Color {
  def apply(r: Float, g: Float, b: Float): Color = Color(r ,g ,b, 1)
  val Clear: Color = Color(0, 0, 0, 0)
  val Black: Color = Color(0, 0, 0, 1)

  implicit val colorMonoidConal: Monoid[Color] =  new Monoid[Color] {
    override def empty: Color = Color.Clear

// ??   overC top bot = top ^+^ (1 - colorA top) *^ bot
    override def combine(top: Color, bottom: Color): Color = {
      val s: Float = 1 - top.a
      val newBottom = Color(bottom.r + s, bottom.g + s, bottom.b + s, bottom.a + s)
      Color(
        top.r + newBottom.r,
        top.g + newBottom.g,
        top.b + newBottom.b,
        top.a + newBottom.a
      )
    }
  }

  implicit val colorMonoidMax: Monoid[Color] =  new Monoid[Color] {
    override def empty: Color = Color.Clear

    //    overC top bot = top ^+^ (1 - colorA top) *^ bot
    override def combine(top: Color, bottom: Color): Color = {
      import Math._
      Color(
        max(top.r, bottom.r),
        max(top.g, bottom.g),
        max(top.b, bottom.b),
        max(top.a, bottom.a),
      )
    }
  }

  implicit val colorMonoidMin: Monoid[Color] =  new Monoid[Color] {
    override def empty: Color = Color(1, 1, 1, 1)

    //    overC top bot = top ^+^ (1 - colorA top) *^ bot
    override def combine(top: Color, bottom: Color): Color = {
      import Math._
      Color(
        min(top.r, bottom.r),
        min(top.g, bottom.g),
        min(top.b, bottom.b),
        min(top.a, bottom.a),
      )
    }
  }
}