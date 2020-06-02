package dimages

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object Dimages {

  type Region = Loc => Boolean

  def rectangleRegion(topLeft: Loc, bottomRight: Loc): Region = {
    loc: Loc =>
      if ((loc.x >= topLeft.x && loc.y >= topLeft.y) &&
        (loc.x < bottomRight.x && loc.y < bottomRight.y)
      )
        true
      else false
  }

  case class Loc(x: Double, y: Double)
  object Loc {
  }

  def loadDimage(path: String): Image = {
    val in = ImageIO.read(new File(path))
    Image(in)

  }

  def saveDimage(width: Int, height: Int, image: Image, file: File): Boolean = {
    def run(img: Image): BufferedImage = {
      val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      for {
        i <- 0 until width
        j <- 0 until height
      } buffer.setRGB(i, j, img.value(Loc(i, j)).value)
      buffer
    }
    ImageIO.write(run(image), "bmp", file)
  }


}
