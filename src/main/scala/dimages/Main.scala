package dimages

import java.io.File

import Dimages._
import Transform._
object Main {

  def main(args: Array[String]): Unit = {

    val baloons = this.getClass.getClassLoader.getResource("baloons.bmp")
    val screen = this.getClass.getClassLoader.getResource("a.bmp")

    val img1: ImageC = Dimages.loadDimage(baloons.getFile)
    val img2: ImageC = Dimages.loadDimage(screen.getFile)


    val over = img1.over(img2, {
      case (c1, c2) => if (c1.brightness > c2.brightness) c1 else c2
    })


    println(Dimages.saveDimage(930, 985, over, createOutputFile("over.bmp")))

    val cropImg = img1.crop(
      Region.rectangle(Loc(300, 500),Loc(600, 800)),
      Color.Clear
    )
    println(Dimages.saveDimage(900, 900, cropImg, createOutputFile("crop.bmp")))



    val rotated: Image[Color] = img1.transform(rotateClockwise(315))
    val rotatedFile: File = createOutputFile("rotated.bmp")
    println(Dimages.saveDimage(900, 900, rotated, rotatedFile))

    val replaced: Image[Color] = img1.cond(
      Region.rectangle(Loc(200, 200), Loc(400, 400)),
      img2
    )
    val replacedFile: File = createOutputFile("replaced.bmp")
    println(Dimages.saveDimage(900, 900, replaced, replacedFile))

 }


}
