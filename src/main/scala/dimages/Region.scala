package dimages

class Region(im: Loc => Boolean) extends Image[Boolean](im)

object Region {
  def rectangle(topLeft: Loc, bottomRight: Loc): Region = new Region({
    loc: Loc =>
      if ((loc.x >= topLeft.x && loc.y >= topLeft.y) &&
        (loc.x < bottomRight.x && loc.y < bottomRight.y)
      )
        true
      else false
  })
}