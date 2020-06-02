package dimages

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

case class Loc(x: Double, y: Double)

object Dimages {

  def loadDimage(path: String): ImageC = {
    val in = ImageIO.read(new File(path))
    ImageC(in)
  }

  def saveDimage(width: Int, height: Int, image: Image[Color], outputFile: File): Boolean = {
    def run(img: Image[Color]): BufferedImage = {
      val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      for {
        i <- 0 until width
        j <- 0 until height
      } buffer.setRGB(i, j, img.im(Loc(i, j)).intColor)
      buffer
    }
    ImageIO.write(run(image), "bmp", outputFile)
  }

  def createOutputFile(path: String): File = {
    val outFile = new File(path)
    if (!outFile.exists()) outFile.createNewFile()
    outFile
  }


}
