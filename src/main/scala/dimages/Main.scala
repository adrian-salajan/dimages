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


//    save[Color](img1, {_.over(img2, Color.colorMonoidMin.combine)}, "over.png")
//    save[Color](img1, {_.crop(
//      Region.rectangle(Loc(300, 500),Loc(600, 800)),
//      Color.Clear
//    )}, "crop.png")
//    save[Color](img1, {_.transform(rotateClockwise(315))}, "rotated.png")
//    save[Color](img1, {_.cond(
//        Region.rectangle(Loc(200, 200), Loc(400, 400)),
//        img2
//    )}, "replaced.png")


    import ColorEffects._

    //functor
//    save[Color](img1, threshold(_, 0.66f, Color.Black, Color.White), "functor/threshold.png")
//    save[Color](img1, grayscale, "functor/grayscale.png")
//    save[Color](img1, saturate, "functor/saturate.png")
//    save[Color](img1, invert, "functor/invert.png")
//    save[Color](img1, keepMax, "functor/keepMax.png")
//    save[Color](img1, replaceColors, "functor/replaceColors.png")
//    save[Color](img1, almostGray, "functor/almostGray.png")
//    save[Color](img1, distributeBlue, "functor/distributeBlue.png")


 }

  def save[A](im: Image[A], m: Image[A] => Image[Color], fileName: String):Unit = {
    val saved = Dimages.saveDimage(900, 900, m(im), createOutputFile(s"png/$fileName"))
    println(s"$fileName: $saved")
  }


}
