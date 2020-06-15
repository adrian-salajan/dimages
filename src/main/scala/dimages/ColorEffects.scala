package dimages

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.Functor

object ColorEffects {
  object F{
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
  object Ap {
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

    def fill(a: Color): Image[Color] = {
      imApplicative.pure(a)
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


  }

  object M {

    import cats.syntax.flatMap._
    import Image.imMonad

    def whiteStripes(a: Image[Color]): Image[Color] =
      a.flatMap(c => new Image({ loc =>
        if (loc.x % 10 == 0) Color.White else c
      }))

    def xx(a: Image[Color]): Image[Color] =
      a.flatMap(c => new Image({ loc =>
        val x = loc.x
        val y = loc.y
        a.im(Loc(Math.tan(x) % 400, y))

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
      image.flatMap(c => new Image({ loc =>
        val newLoc = t(loc)
        image.im(newLoc)
      }))
    }

  }
}
