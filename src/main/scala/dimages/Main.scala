package dimages

import java.io.File

import Dimages._
import Transform._
object Main {

  def main(args: Array[String]): Unit = {

    val baloons = this.getClass.getClassLoader.getResource("baloons.bmp")
    val screen = this.getClass.getClassLoader.getResource("a.bmp")

    val img1: Image[Color] = Dimages.loadDimage(baloons.getFile)
    val img2: Image[Color] = Dimages.loadDimage(screen.getFile)


    val over = img1.over(img2, Color.colorMonoidMin.combine)


    println(Dimages.saveDimage(930, 985, over, createOutputFile("over.png")))

    val cropImg = img1.crop(
      Region.rectangle(Loc(300, 500),Loc(600, 800)),
      Color.Clear
    )
    println(Dimages.saveDimage(900, 900, cropImg, createOutputFile("crop.png")))



    val rotated: Image[Color] = img1.transform(rotateClockwise(315))
    val rotatedFile: File = createOutputFile("rotated.png")
    println(Dimages.saveDimage(900, 900, rotated, rotatedFile))

    val replaced: Image[Color] = img1.cond(
      Region.rectangle(Loc(200, 200), Loc(400, 400)),
      img2
    )
    val replacedFile: File = createOutputFile("replaced.png")
    println(Dimages.saveDimage(900, 900, replaced, replacedFile))

    import cats.syntax.all._
    import Image.imFunctor

    val fullRed = img1.map(c => c.r == 1).map(b => if (b) Color.Red else Color.Clear)
    println(Dimages.saveDimage(900, 900, fullRed, createOutputFile("fullRed.png")))
 }


}
