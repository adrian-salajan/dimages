package dimages

import cats.kernel.Monoid


case class Color(red: Float, green: Float, blue: Float, alpha: Float) {
  def isWhiteish: Boolean = isGrayish && brightness > 0.85

  val brightness: Float = (red + green + blue) / 3

  val toInt: Int = (((alpha * 255).toInt & 0xFF) << 24) |
    (((red * 255).toInt & 0xFF) << 16) |
    (((green * 255).toInt & 0xFF) << 8) |
    (((blue * 255).toInt & 0xFF) << 0)

  def toGray: Color = new Color(brightness, brightness, brightness, 1)

  def maxColor: Color = {
    if (red == green && green == blue) this else
    if (this == Color.White) Color.White else
    if (this == Color.Black) Color.Black
    else
    List(red, green, blue).zipWithIndex.maxBy(_._1)._2 match {
      case 0 => Color.Red
      case 1 => Color.Green
      case 2 => Color.Blue
    }
  }

  def isGrayish: Boolean = {
    Color.aproxEq(red, green, 0.15f) &&
    Color.aproxEq(red, blue, 0.15f) &&
    Color.aproxEq(green, blue, 0.15f)
  }



  def - (a: Color): Color =  Color.applyf(this, a)((a, b) => Math.max(0f, a - b) )

  def + (a: Color): Color = Color.applyf(this, a)((a, b) => Math.min(1, a + b) )


  def overlay(a: Color): Color = Color.applyf(this, a)(overlay)



  private def overlay(a: Float, b: Float): Float =
    if (a < 0.5) 2*a*b
    else 1-2*(1-a)*(1-b)

}


object Color {
  def apply(r: Float, g: Float, b: Float): Color = Color(r ,g ,b, 1)
  val Clear: Color = Color(0, 0, 0, 0)
  val Black: Color = Color(0, 0, 0, 1)
  val White: Color = Color(1, 1, 1, 1)
  val Red: Color = Color(1, 0, 0, 1)
  val Green: Color = Color(0, 1, 0, 1)
  val Blue: Color = Color(0, 0, 1, 1)

  def applyf(a: Color, b: Color)(f: (Float, Float) => Float): Color =
    Color(
      f(a.red, b.red),
      f(a.green, b.green),
      f(a.blue, b.blue),
      1
    )

  def aproxEq(a: Float, b: Float, tolerance: Float): Boolean = {
    if (a == b) true
    else if (a < b) a + tolerance >= b
    else b + tolerance >= a
  }

  def mono(f: Float): Color = Color(f, f, f, 1)

  implicit val colorMonoidConal: Monoid[Color] =  new Monoid[Color] {
    override def empty: Color = Color.Clear

// ??   overC top bot = top ^+^ (1 - colorA top) *^ bot
    override def combine(top: Color, bottom: Color): Color = {
      val s: Float = 1 - top.alpha
      val newBottom = Color(bottom.red + s, bottom.green + s, bottom.blue + s, bottom.alpha + s)
      Color(
        top.red + newBottom.red,
        top.green + newBottom.green,
        top.blue + newBottom.blue,
        top.alpha + newBottom.alpha
      )
    }
  }

  implicit val colorMonoidMax: Monoid[Color] =  new Monoid[Color] {
    override def empty: Color = Color.Clear

    //    overC top bot = top ^+^ (1 - colorA top) *^ bot
    override def combine(top: Color, bottom: Color): Color = {
      import Math._
      Color(
        max(top.red, bottom.red),
        max(top.green, bottom.green),
        max(top.blue, bottom.blue),
        max(top.alpha, bottom.alpha),
      )
    }
  }

  implicit val colorMonoidMin: Monoid[Color] =  new Monoid[Color] {
    override def empty: Color = Color(1, 1, 1, 1)

    //    overC top bot = top ^+^ (1 - colorA top) *^ bot
    override def combine(top: Color, bottom: Color): Color = {
      import Math._
      Color(
        min(top.red, bottom.red),
        min(top.green, bottom.green),
        min(top.blue, bottom.blue),
        min(top.alpha, bottom.alpha),
      )
    }
  }
}