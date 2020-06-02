package dimages

import java.awt.image.BufferedImage

import dimages.Dimages.{Loc, Region}

case class Image(value: Loc => Color) {

  def over(img2: Image, combine: (Color, Color) => Color): Image =
    Image { loc: Loc =>
      combine(
        value(loc),
        img2.value(loc)
      )
  }

  def crop(reg: Region): Image = Image { loc =>
    if (reg(loc)) value(loc) else Color.Clear
  }

}

object Image {
  def apply(b: BufferedImage): Image = new Image({
    loc: Loc => {
      Color(b.getRGB(loc.x.toInt, loc.y.toInt))
    }
  })

  def monochrome(color: Color): Image = Image { _ =>
    color
  }


}
