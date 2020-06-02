package dimages

case class Color(intColor: Int) {
  val color = new java.awt.Color(intColor)
  def red: Int = color.getRed
  def green: Int = color.getGreen
  def blue: Int = color.getBlue
  def alpha: Int = color.getAlpha

  val brightness: Int = (red + green + blue) / 3
}
object Color {
  def apply(r: Int, g: Int, b: Int): Color = Color((r * 65536) + (g * 256) + b)
  val Clear: Color = Color(new java.awt.Color(0f, 0f, 0f, 0f).getRGB)
  val Black: Color = Color(0, 0, 0)
}