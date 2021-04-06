package dimages

import java.io.File
import Dimages._
import cats.Applicative
import dimages.Image.imApplicative
object Main {

  def main(args: Array[String]): Unit = {

    val birdie = this.getClass.getClassLoader.getResource("bird.png")
    val cray = this.getClass.getClassLoader.getResource("crayons.png")

    val bird: Image[Color] = Dimages.loadDimage(birdie.getFile)
    val crayons: Image[Color] = Dimages.loadDimage(cray.getFile)

    def identity[A]: Image[A] => Image[A] = a => a


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
//    runFunctorEffects()
//    runApplicativeEffects()
      runMonadEffects()





    //comonad
//    save[Color](CM.average(bird), identity, "comonad/average.png")
//    save[Color](CM.brightest(bird), identity, "comonad/brightest.png")
//    save[Color](CM.pick(bird, Loc(300, 180)), identity, "comonad/pick.png")
//    save[Color](CM.greyscale(bird), identity, "comonad/grayscale.png")

//    import cats.syntax.comonad._
//    import cats.syntax.coflatMap._
//    save[Color](bird.coflatten.coflatten.coflatten
//      .coflatMap(iic => new Image[Color]({
//      loc => iic.im(Loc(loc.x, loc.y)).im(Loc(loc.x, loc.y))
//    }))
//      .extract.extract.extract
//
//      , identity, "comonad/coflatten.png")



    def runFunctorEffects() = {
      save[Color](bird, FunctorEffect.threshold(_, 0.4f, Color.Black, Color.White), "functor/threshold.png")
      save[Color](bird, FunctorEffect.grayscale, "functor/grayscale.png")
      save[Color](bird, FunctorEffect.saturate, "functor/saturate.png")
      save[Color](bird, FunctorEffect.invert, "functor/invert.png")
      save[Color](bird, FunctorEffect.keepMax, "functor/keepMax.png")
      save[Color](bird, FunctorEffect.replace(_, Color(36 / 255f,22 / 255f, 22/255f,1), Color.Green), "functor/replaceColors.png")
      save[Color](bird, FunctorEffect.almostGray, "functor/almostGray.png")
      save[Color](bird, FunctorEffect.distributeBlue, "functor/distributeBlue.png")
      save[Color](bird, FunctorEffect.ignoreInputAndReplaceWith(_, Color.Green), "functor/ignoreInputReplaceGreen.png")
    }

    def runApplicativeEffects() = {
      save[Color](ApplicativeEffect.substract(bird, crayons), identity, "applicative/substract.png")
      save[Color](ApplicativeEffect.max(bird, crayons), identity, "applicative/max.png")
      save[Color](ApplicativeEffect.overlay(bird, crayons), identity, "applicative/overlay.png")
      save[Color](ApplicativeEffect.recolor(bird, crayons), identity, "applicative/recolor.png")
      save[Color](
        Image.imApplicative.map(
          Image.imApplicative
            .map2(bird, crayons)((b, c) => (b, c))
        )(tc => if (math.random() < 0.5) tc._1 else tc._2)

        , identity, "applicative/disolve2.png")

      //as a simple functor
       save[Color](ApplicativeEffect.fill(Color.Green), identity, "applicative/fill.png")


      //apply
      save[Color](bird, ApplicativeEffect.apply(_, Image.imApplicative.pure[Color => Color](
        x => {
          val b = x.brightness
          Color(b, b, b)
        }
      )), "applicative/colorifyBird1.png")


      def psychedelics: Image[Color => Color] = {
        new Image[Color => Color](
          loc => c => loc match {
            case Loc(a, b) => Color(
              (1f * Math.sin(a / 20) + b / 300).toFloat,
              c.green,
              (1f * Math.cos(b / 40) + a / 150).toFloat,
            )
          }
        )
      }
      save[Color](bird, ApplicativeEffect.apply(_, psychedelics), "applicative/colorifyBird2.png")

      save[Color](Image[Color](Color.Black), ApplicativeEffect.apply(_, new Image[Color => Color](
        loc => c => loc match {
          case Loc(a, b) => Color(
            (1f  * Math.sin(a / 20) + b/300).toFloat,
            c.green,
            (1f * Math.cos(b / 40) + a/150).toFloat,
          )
        }
      )), "applicative/colorifyNoBird2.png")

      save[Color](bird, ApplicativeEffect.apply(_, checkerPattern), "applicative/colorifyBird3.png")

      {
        val psychedelicsImage = imApplicative.ap(psychedelics)(imApplicative.pure(Color.Clear))
        save[Color](psychedelicsImage, identity, "applicative/psychedelics.png")
      }
      save[Color](bird, ApplicativeEffect.apply(_, psychedelics), "applicative/psychedelicsBird.png")

      def checkerPattern: Image[Color => Color] = {
        new Image[Color => Color](
          loc => c => loc match {
            case Loc(a, b) =>
              if (a % 10 < 5 && b % 10 < 5) Color.White
              else if (a % 10 >= 5 && b % 10 >= 5) Color.Black
              else c
          }
        )
      }



      save[Color](Image.imApplicative.map2(bird,
        checkerPattern)((a, b) =>
        b(a)
      ),
        identity, "applicative/colorifyBirdPlusNoBird3.png"
      )



      //
      //    save[Color](Ap.colorify(
      //      Image.circle(300, 200, 180),
      //      Image( (b: Boolean) => if (b) Color.White else Color.Black )
      //    ),
      //      identity, "applicative/circle.png"
      //    )
      //rasterization problem

      //    save[Color](Ap.colorify2[Boolean](
      //      Image.circle(300, 200, 180),
      //      Image.apply(Color.Black),
      //      (b, c) => if (b) Color.White else c
      //    ),
      //      identity, "applicative/circle2.png"
      //    )
      //rasterization problem

      //    save[Color](Ap.overlapIf(bird, crayons, {
      //      c => c.isWhiteish
      //    }), identity, "applicative/seeThroughWhite.png")

      //    save[Color](Ap.disolve(bird, crayons), identity, "applicative/disolve.png")
    }

    def runMonadEffects() = {
//      save[Color](ApplicativeEffect.bandsFromOriginalImage(bird), identity, "applicative/bandsFromOriginalImage.png")

      //Monad
      import cats.syntax.flatMap._
      save[Color](
          Image.redCircle(Color.Green, 300, 200, 200)
            .flatMap(Image.stripes(Color.White, _))
            .flatMap(Image.redCircle(_, 500, 110, 200))
            .flatMap(Image.colorWithBlackBorder),
        identity, "monad/monad-border-and-circle.png")

      save[Color](
        bird.flatMap(birdColor => Image.redCircle(birdColor, 300, 200, 200).flatMap(circleColor => Image.topHalf(birdColor, circleColor)))
          .flatMap(imageUpUntilNowColor => crayons.flatMap(Image.stripes(_, imageUpUntilNowColor)).flatMap(Image.topHalf(_, imageUpUntilNowColor)))
          .flatMap(imageUpUntilNowColor => Image.redCircle(imageUpUntilNowColor, 500, 110, 200))
          .flatMap(imageUpUntilNowColor => Image.colorWithBlackBorder(imageUpUntilNowColor)),
        identity, "monad/birdie-border-and-circle.png")

//      save[Color](MonadEffect.bandsFromOriginalImage(bird), identity, "monad/bands.png")
      //    save[Color](M.transforms(bird,
      //      M.translate(-320, -213),
      //      M.scale(2, 0.2),
      //      M.rotate(45),
      //      M.translate(320, 213),
      //    ), identity, "monad/rotate.png")

      //    save[Color](M.combine(bird, crayons), identity, "monad/combine.png")
      //    save[Color](M.swirl(bird), identity, "monad/swirl.png")
      //    save[Color](M.average(bird), identity, "monad/average.png")
    }
 }

  def save[A](im: Image[A], toColor: Image[A] => Image[Color], fileName: String):Unit = {
    val saved = Dimages.saveDimage(640, 426, toColor(im), createOutputFile(s"png/$fileName"))
    println(s"$fileName: $saved")
  }




}
