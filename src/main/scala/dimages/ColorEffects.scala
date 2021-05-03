package dimages

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.Functor

object ColorEffects {
  object FunctorEffect{
    import cats.syntax.functor._
    import Image.imFunctor
    //functor

import cats.instances.function._


    def threshold(img: Image[Color], threshhold: Float, below: Color, over: Color): Image[Color] =
      img.map(c => if (c.brightness < threshhold) below else over)

    def grayscale(img: Image[Color]): Image[Color] =
      img.map(_.toGray)

    def replace(img: Image[Color], color: Color, replacement: Color): Image[Color] =
      img.map(c => if (c == color) replacement else c)

    def invert(img: Image[Color]): Image[Color] =
      img.map(c => Color(1 - c.red, 1 - c.green, 1 - c.blue))

    def saturate(img: Image[Color]): Image[Color] =
      img.map {
        case Color(r, g, b, a) =>
          val index = List(r, g, b).zipWithIndex.maxBy(_._1)._2
          index match {
            case 0 => Color(1, g, b, a)
            case 1 => Color(r, 1, b, a)
            case 2 => Color(r, g, 1, a)
          }
      }

    def keepMax(img: Image[Color]): Image[Color] =
      img.map(c => c.maxColor match {
        case Color.Red => Color(c.red, 0, 0, 1)
        case Color.Green => Color(0, c.green, 0, 1)
        case Color.Blue => Color(0, 0, c.blue, 1)
        case _ => c
      })

    def swapColors(img: Image[Color]): Image[Color] = {
      import Math._
      img.map { c =>
        Color(
          c.blue,
          c.red,
          c.green
        )
      }
    }

    def distributeBlue(img: Image[Color]): Image[Color] = {
      import Math._
      img.map { c =>
        if (c.isGrayish) c else {
          val hb = c.blue / 3
          Color(
            min(1, c.red + hb),
            min(1, c.green + hb),
            hb
          )
        }
      }
    }

    def almostGray(img: Image[Color]): Image[Color] = {
      img.map { c =>
        if (c.isGrayish) Color(0.3f, 0.3f, 0.3f, 1f)
        else c
      }
    }

    def ignoreInputAndReplaceWith(img: Image[Color], c: Color): Image[Color] = img.map(_ => c)
  }

  //applicative
  object ApplicativeEffect {
    import cats.syntax.apply._
    import Image.imApplicative

    def substract(img: Image[Color], img2: Image[Color]): Image[Color] = {
      img.map2(img2) {
        case (a, b) => a - b
      }
    }

    def max(a: Image[Color], b: Image[Color]): Image[Color] = a.map2(b) {
      case (a, b) => if (a.brightness > b.brightness) a else b
    }

    def overlay(a: Image[Color], b: Image[Color]): Image[Color] = a.map2(b) {
      case (a, b) => a.overlay(b)
    }

    def recolor(a: Image[Color], b: Image[Color]): Image[Color] =
      a.map2(b) {
      case (a, b) =>
        Color(a.brightness * b.red, a.brightness * b.green, a.brightness * b.blue)
    }

    def fill(a: Color): Image[Color] = {
      imApplicative.pure(a)
    }

    def apply[A](img: Image[Color], colorEff: Image[Color => Color]): Image[Color] = {
      imApplicative.ap(colorEff)(img)
    }


    def colorify[A](img: Image[A], colorEff: Image[A => Color]): Image[Color] = {
      imApplicative.ap(colorEff)(img)
    }

    def colorify2[A](img: Image[A], imgb: Image[Color], f: (A, Color) => Color): Image[Color] = {
     imApplicative.map2(img, imgb)(f)
    }

    def overlapIf(imga: Image[Color], imgb: Image[Color], f: Color => Boolean): Image[Color] = {
      val effect: Color => Color => Color = a => b => if (f(b)) a else b
      val ap1 = imApplicative.ap(imApplicative.pure(effect))(imga)
      val ap2 = imApplicative.ap(ap1)(imgb)
      ap2

    }

    def disolve(imga: Image[Color], imgb: Image[Color]): Image[Color] =
      overlapIf(imga, imgb, _ => Math.random() > 0.5)

// how can this be done with Applicatives or monads ??
//    def bandsFromOriginalImage(a: Image[Color]): Image[Color] = {
//      val bands = new Image( loc =>
//        a.im(Loc(loc.x, loc.y % 100))
//      )
//      bands
//    }





  }

  object MonadEffect {
    import cats.syntax.flatMap._
    import cats.syntax.monad._
    import Image.imMonad

    //by replacing color c with transparent can achieve the same effect with Applicative
    def whiteStripes(a: Image[Color]): Image[Color] =
      a.flatMap(c => new Image({ loc =>
        if (loc.x % 10 == 0) Color.White else c
      }))

    def bandsFromOriginalImage(a: Image[Color]): Image[Color] =
      a.flatMap(_ => new Image({ loc =>
        a.im(Loc(loc.x, loc.y % 100))
      }))

    def swirl(a: Image[Color]): Image[Color] =
      a.flatMap(_ => new Image({ loc =>
        val du = 300
        val dv = 300
        def theta(u: Double, v: Double) = (Math.PI * (Math.pow(Math.pow(u - du,2) + Math.pow(v - dv, 2), 0.5))) / 128
        val t = theta(loc.x, loc.y)
        def fx(u: Double, v: Double) = (u - du) * Math.cos(t) + (v - dv) * Math.sin(t) + du
        def fy(u: Double, v: Double) = -(u - du) * Math.sin(t) + (v - dv) * Math.cos(t) + dv
        val x = fx(loc.x, loc.y)
        val y = fy(loc.x, loc.y)
        a.im(Loc(x, y))
      }))

    def combine(a: Image[Color], b: Image[Color]): Image[Color] =
      a.flatMap(c => new Image({ loc =>
        val x = loc.x
        if (x < 300)
          c
        else b.im(loc)

      }))

    type Transformation = Loc => Loc

    def translate(x: Double, y: Double): Loc => Loc = loc => {
      val r = DenseMatrix((loc.x), (loc.y)) + DenseMatrix((x), (y))
      Loc(r(0, 0), r(1, 0))
    }

    def rotate(a: Double): Loc => Loc = loc => {
      val r = DenseMatrix((loc.x, loc.y)) * DenseMatrix((Math.cos(a), Math.sin(a)), (-Math.sin(a), Math.cos(a)))
      Loc(r(0, 0), r(0, 1))
    }

    def scale(x: Double, y: Double): Loc => Loc = loc => {
      val s = DenseMatrix((loc.x), (loc.y)).t * DenseMatrix((1/x, 0d), (0d, 1/y))

      Loc(s.t.apply(0,0), s.t.apply(1, 0))
    }


    def transforms(image: Image[Color], ts: (Loc => Loc)*): Image[Color] = {
      val t: Loc => Loc = ts.toList match {
        case List() => a => a
        case _ => ts.reverse.reduce[Loc => Loc] {
          case (a, b) => a.compose(b)
        }
      }
      image.flatMap(_ => new Image({ loc =>
        val newLoc = t(loc)
        image.im(newLoc)
      }))
    }


//    def average(a: Image[Color]): Image[Color] = {
//      a.flatMap(c => new Image[Color]({
//        loc => Image.avg(a)
//      }))
//    }



    def drawBorder(a: Image[Color]): Image[Color] = {
      a.flatMap(Image.addBlackBorder)
    }

    def drawCircle(a: Image[Color]): Image[Color] = {
      a.flatMap(Image.redCircle(_, 300, 200, 250))
    }
  }

  object ComonadEffects {
//    import cats.syntax.coflatMap._
//    import cats.syntax.comonad._
    import cats.implicits._

    def point(a: Image[Color]): Color = a.extract


    def average(a: Image[Color], squareSizeInPixels: Int): Image[Color] =
      a.coflatMap(img => Image.regionAverage(img, squareSizeInPixels, squareSizeInPixels))

    def translate(a: Image[Color], x: Int, y: Int): Image[Color] =
      a.coflatMap(img =>
        img.im(Loc(x, y))
      )

    def brightest(a: Image[Color], width: Int, height: Int): Image[Color] =
      a.coflatMap(Image.regionBrightest(_, width, height)
    )

    def enhanceContrast(a: Image[Color], width: Int, height: Int): Image[Color] = {

      a.coflatMap { img =>
        val avg = Image.regionAverage(img, width, height).brightness
        val local = Image.imComonad.extract(img)
        if (local.brightness > avg) {
          print(true)
          Color(
            Math.max(local.red * 1.3, 1),
            Math.max(local.green * 1.3, 1),
            Math.max(local.blue * 1.3, 1),
            1f
          )
        } else
          Color(
            local.red * 0.5,
            local.green * 0.5,
            local.blue * 0.5,
            1f
          )
      }
    }

      def convolution(a: Image[Color], kernel: DenseMatrix[Double], divisor: Double = 1): Color = {
//        val sumKernel = kernel.toArray.map(Math.signum).sum
//        val F = if (sumKernel == 0) 1 else sumKernel

        val convolutedChannels = Image.matrix(a, kernel.cols, kernel.rows) !* kernel

        val red = convolutedChannels.red.toArray.sum / divisor
        val green = convolutedChannels.green.toArray.sum / divisor
        val blue = convolutedChannels.blue.toArray.sum / divisor
        Color(red, green, blue)
      }
      def gausianBlur(a: Image[Color]) =
        a.coflatMap(img => convolution(img, Image.gausianBlur))
          .coflatMap(img => convolution(img, Image.gausianBlur))

     def edgeDetect(a: Image[Color]) =
        Image.imComonad.map(a)(_.toGray).coflatMap(img => convolution(img, Image.edgeDetect))

     def emboss(a: Image[Color]) =
         a.coflatMap(img => convolution(img, Image.emboss))




      //    def brightest(a: Image[Color]): Image[Color] = a.coflatMap(Image.brightest)
      //
      //    def pick(a: Image[Color], loc: Loc): Image[Color] = a.coflatMap(img => {
      //      img.im(loc)
      //    })
      //
      //    def greyscale(a: Image[Color]): Image[Color] =Image.imComonad.map(a)(c => Color(c.brightness, c.brightness, c.brightness, 1))
      //
      //
      //
      //      val im = a.coflatten
      //      val x = Image.imComonad.map(im)(imc => new Image[Color]({
      //        loc => if (loc.x % 100 == 0) Color.White else imc.im(loc)
      //      }))
      //      x.extract
    }
}
