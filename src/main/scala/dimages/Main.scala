package dimages

import java.io.File

import Dimages._
object Main {

  def main(args: Array[String]): Unit = {

    val birdie = this.getClass.getClassLoader.getResource("bird.png")
    val cray = this.getClass.getClassLoader.getResource("crayons.png")

    val bird: Image[Color] = Dimages.loadDimage(birdie.getFile)
    val crayons: Image[Color] = Dimages.loadDimage(cray.getFile)


//    save[Color](bird, {_.over(img2, Color.colorMonoidMin.combine)}, "over.png")
//    save[Color](bird, {_.crop(
//      Region.rectangle(Loc(300, 500),Loc(600, 800)),
//      Color.Clear
//    )}, "crop.png")
//    save[Color](bird, {_.transform(rotateClockwise(315))}, "rotated.png")
//    save[Color](bird, {_.cond(
//        Region.rectangle(Loc(200, 200), Loc(400, 400)),
//        img2
//    )}, "replaced.png")


    import ColorEffects._

    //functor
//    save[Color](bird, F.threshold(_, 0.4f, Color.Black, Color.White), "functor/threshold.png")
//    save[Color](bird, F.grayscale, "functor/grayscale.png")
//    save[Color](bird, F.saturate, "functor/saturate.png")
//    save[Color](bird, F.invert, "functor/invert.png")
//    save[Color](bird, F.keepMax, "functor/keepMax.png")
//    save[Color](bird, F.replaceColors, "functor/replaceColors.png")
//    save[Color](bird, F.almostGray, "functor/almostGray.png")
//    save[Color](bird, F.distributeBlue, "functor/distributeBlue.png")

    //applicative
    def self[A]: Image[A] => Image[A] = a => a
//    save[Color](Ap.substract(bird, crayons), self, "applicative/substract.png")
//    save[Color](Ap.max(bird, crayons), self, "applicative/max.png")
//    save[Color](Ap.overlay(bird, crayons), self, "applicative/overlay.png")
//    save[Color](Ap.fill(Color.Green), self, "applicative/fill.png")
//    save[Color](Ap.colorify(
//      Image.self,
//      Image((loc: Loc) => Color.mono(Math.sin(loc.x).toFloat) )
//    ),
//    self, "applicative/create3.png")
//
    save[Color](Ap.colorify(
      Image.circle(300, 200, 180),
      Image( (b: Boolean) => if (b) Color.White else Color.Black )
    ),
      self, "applicative/circle.png"
    ) //rasterization problem

    save[Color](Ap.overlapIf(bird, crayons, {
      c => c.isWhiteish
    }), self, "applicative/seeThroughWhite.png")

    save[Color](Ap.disolve(bird, crayons), self, "applicative/disolve.png")


 }

  def save[A](im: Image[A], m: Image[A] => Image[Color], fileName: String):Unit = {
    val saved = Dimages.saveDimage(640, 426, m(im), createOutputFile(s"png/$fileName"))
    println(s"$fileName: $saved")
  }



}
