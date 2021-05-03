package dimages


import breeze.linalg.DenseMatrix

import java.awt.image.BufferedImage
import cats.kernel.Monoid
import cats.{Applicative, Comonad, Functor, Monad}

import scala.annotation.tailrec
import scala.util.Try

class Image[A](val im: Loc => A) {

  def transform(tr: Loc => Loc): Image[A] = new Image[A]({
    loc: Loc =>  im(tr(loc))
    })

  def over(img2: Image[A], combine: (A, A) => A): Image[A] =
    Image.lift2(combine.curried)(this)(img2)

  def crop(reg: Image[Boolean], id: A): Image[A] =
    Image.lift1{ l: Loc => if (reg.im(l)) im(l) else id}(
      new Image[Loc]({ //identity (of what?)
        loc => loc
      })
    )

  def cond(mask: Image[Boolean], other: Image[A]): Image[A] =
    Image.lift3[Boolean, A, A, A] {
      bo => ca => cb => {
        if (bo)
        ca
      else cb
    }}(mask)(this)(other)

}

object Image {
  def lift[A](a: A): Image[A] = new Image[A]({
    _: Loc => a
  })

  def lift1[A, B](f: A => B): Image[A] => Image[B] =
    ima => new Image[B]({
      loc => f(ima.im(loc))
    })

  def lift2[A, B, C](f: A => B => C): Image[A] => Image[B] => Image[C] =
    ima => imab => new Image[C]({
      loc => f(ima.im(loc))(imab.im(loc))
    })

  def lift3[A, B, C, D](f: A => B => C => D): Image[A] => Image[B] => Image[C] => Image[D] =
    ima => imab => imac => new Image[D]({
      loc => f(ima.im(loc))(imab.im(loc))(imac.im(loc))
    })

  def apply[A](a: A): Image[A] = imApplicative.pure(a)

  def self: Image[Loc] = new Image({ a => a })

  def load(b: BufferedImage): ImageC = new ImageC({
    loc: Loc => {

      Try( //map out of bounds to black
        {
          val c = new java.awt.Color(b.getRGB(loc.x.toInt, loc.y.toInt))
          Color(c.getRed.toDouble / 255, c.getGreen.toDouble / 255, c.getBlue.toDouble / 255, c.getAlpha.toDouble / 255)
        }
      ).fold(_ => Color.Black, c => c)
    }
  })

  def monochrome[A](a: A): Image[A] = Image.lift(a)

  implicit def imMonoid[A](implicit M: Monoid[A]): Monoid[Image[A]] = new Monoid[Image[A]] {
    override def empty: Image[A] = Image.lift(M.empty)

    override def combine(x: Image[A], y: Image[A]): Image[A] = {
      val c: A => A => A = (M.combine _).curried
      val i = Image.lift2(c)(x)(y)
      i
    }
  }

  implicit val imFunctor: Functor[Image] = new Functor[Image] {
    override def map[A, B](fa: Image[A])(f: A => B): Image[B] = Image.lift1(f)(fa)
  }

  val imFunctorDirectly: Functor[Image] = new Functor[Image] {
    override def map[A, B](fa: Image[A])(f: A => B): Image[B] = new Image[B](loc =>
      f(fa.im(loc))
    )
  }

  implicit val imApplicative: Applicative[Image] = new Applicative[Image] {
    override def pure[A](x: A): Image[A] = Image.lift(x)

    override def ap[A, B](ff: Image[A => B])(fa: Image[A]): Image[B] = {
      Image.lift2[A => B, A, B] {
        fab => a => fab(a)
      }(ff)(fa)
    }

  }

  implicit val imApplicativeDirectly: Applicative[Image] = new Applicative[Image] {
    override def pure[A](x: A): Image[A] = Image.lift(x)

    override def ap[A, B](ff: Image[A => B])(fa: Image[A]): Image[B] = {
      new Image[B]( loc =>
        ff.im(loc)(fa.im(loc))
      )
    }

    //equivalence of ap VS map2
    def map2ViaApAndPure[A, B, C](fa: Image[A], fb: Image[B], f: (A, B) => C): Image[C] = {
      val one = ap(pure(f.curried))(fa)
      ap(one)(fb)
    }
    def apViaMap2[A, B](ff: Image[A => B])(fa: Image[A]): Image[B] =
      map2Impl(ff, fa)((f, a) => f(a))

    //Applicative definition based on map2
    def map2Impl[A, B, C](fa: Image[A], fb: Image[B])(f: (A, B) => C): Image[C] = new Image[C] (
      loc =>
        f(fa.im(loc), fb.im(loc))
      )

    def mapViaApAndPure[A, B](fa: Image[A])(f: A => B): Image[B] =
      ap(pure(f))(fa)
  }
  implicit val imMonad: Monad[Image] = new Monad[Image] {
    override def flatMap[A, B](fa: Image[A])(f: A => Image[B]): Image[B] = new Image[B]({
      loc =>
        val img: Image[B] = f(fa.im(loc))
        img.im(loc)
    })

    override def tailRecM[A, B](a: A)(f: A => Image[Either[A, B]]): Image[B] = new Image[B]({
      loc =>
        @tailrec
        def rec(ab: Either[A, B]): B = ab match {
          case Left(a) => rec(f(a).im(loc))
          case Right(b) => b
        }

        rec(f(a).im(loc))
    })


    override def pure[A](x: A): Image[A] = new Image[A]({
      _: Loc => x
    })

      //equivalence of flatMap VS map+flatten
      def flatMapViaMapAndFlatten[A, B](fa: Image[A])(f: A => Image[B]): Image[B] = flattenImpl(mapImpl(fa)(f))
      def flattenViaFlatMap[F[_], A](ffa: Image[Image[A]]): Image[A] = flatMap(ffa)(a => a)

      //Monad definition based on map and flatten
      def mapImpl[A, B](fa: Image[A])(f: A => B): Image[B] = new Image[B](loc =>
        f(fa.im(loc))
      )

      def flattenImpl[F[_], A](ffa: Image[Image[A]]): Image[A] = new Image[A](
        loc =>
          ffa.im(loc).im(loc)
      )


  }

   implicit val imComonad: Comonad[Image] = new Comonad[Image] {
     override def extract[A](x: Image[A]): A = x.im(Loc(0, 0))

     override def coflatMap[A, B](fa: Image[A])(f: Image[A] => B): Image[B] = {
       new Image[B](lb =>
         f(
           new Image[A](la =>
             fa.im(Loc(lb.x + la.x, lb.y + la.y)) //lb = left identity, la = right identity
           )
         )
       )
     }

     override def map[A, B](fa: Image[A])(f: A => B): Image[B] = {
       coflatMap(fa)(img => f(extract(img)))
     }

      //Comonad definition based on map and coflatten
     def mapImpl[A, B](fa: Image[A])(f: A => B): Image[B] = {
       new Image[B](
         loc => f(fa.im(loc))
       )
     }

    def coflattenImpl[A](fa: Image[A]): Image[Image[A]] =
      new Image[Image[A]] (
         la => new Image[A](
           lb => fa.im(Loc(lb.x + la.x, lb.y + la.y))
         )
      )

     def coflatMapViaCoflatten[A, B](fa: Image[A])(f: Image[A] => B): Image[B] =
       mapImpl(coflattenImpl(fa))(f)

   }


  def regionAverage(i: Image[Color], width: Int, height: Int): Color = {
    val samples = for {
      x <- rangeCenter0(width)
      y <- rangeCenter0(height)
    } yield (x, y)
    val total = samples
      .map(loc => i.im(Loc(loc._1, loc._2)))

    colorAverage(total.toList)
  }

  def colorAverage(color: List[Color]): Color = {
    val newRed = channelColorAverage(color.map(_.red).toArray)
    val newGreen = channelColorAverage(color.map(_.green).toArray)
    val newBlue = channelColorAverage(color.map(_.blue).toArray)
    Color(newRed, newGreen, newBlue)
  }

  def channelColorAverage(channelToAverage: Array[Double]): Double = {
    Math.sqrt(
      channelToAverage.map(Math.pow(_, 2)).sum / channelToAverage.length
    )
  }

  def channelColorAverage2(channelToAverage: Array[Double]): Double = {
    channelToAverage.sum / channelToAverage.length
  }

  def regionBrightest(i: Image[Color], width: Int, height: Int): Color = {
    val samples = for {
      x <- if (width < 2 ) Range(0, width, 1) else Range(0 - width / 2, width / 2 , 1)
      y <- if (height < 2) Range(0, height, 1) else Range(0 - height / 2, height / 2, 1)
    } yield (x, y)
    val max = samples.map(n => i.im(Loc(n._1, n._2))).reduce((a, b) => if (a.brightness > b.brightness) a else b)
    max
  }

    case class ColorAreaMatrix(red: DenseMatrix[Double], green: DenseMatrix[Double], blue: DenseMatrix[Double]) {
      def !*(kernel: DenseMatrix[Double]) =
        ColorAreaMatrix(red *:* kernel, green *:* kernel, blue *:* kernel)

      def /(scalar: Double) =
        ColorAreaMatrix(red / scalar, green / scalar, blue / scalar)

    }

    def matrix(a: Image[Color], width: Int, height: Int): ColorAreaMatrix = {
      val samples = for {
        x <- rangeCenter0(width)
        y <- rangeCenter0(height)
      } yield a.im(Loc(x, y))
      val areaRed = new DenseMatrix[Double](height, width, samples.map(_.red), 0)
      val areaGreen = new DenseMatrix[Double](height, width, samples.map(_.green), 0)
      val areaBlue= new DenseMatrix[Double](height, width, samples.map(_.blue), 0)
      ColorAreaMatrix(areaRed, areaGreen, areaBlue)
    }

    val id = new DenseMatrix[Double](
    3,
    3,
    Array[Double](0, 0, 0, 0, 1, 0, 0, 0, 0), 0
  )

  val gausianBlur = new DenseMatrix[Double](
    3,
    3,
    Array[Double](1, 2, 1, 2, 4, 2, 1, 2, 1), 0
  ) *= 1d/16

    val edgeDetect = new DenseMatrix[Double](
    3,
    3,
    Array[Double](
      0, -1, 0,
      -1, 4, -1,
      0, -1, 0), 0
  ) *= 1d/32

      val edgeDetect2= new DenseMatrix[Double](
    3,
    3,
    Array[Double](
      -1, 0, 2,
      -2, 0, 2,
      -1, 0, 1), 0
  ) *= 1d/8

        val emboss= new DenseMatrix[Double](
    3,
    3,
    Array[Double](
      -2, -1, 0,
      -1, 1, 1,
      0, 1, 2), 0
  )

  private def rangeCenter0(length: Int): Array[Int] =
    length match {
      case 1 => Array[Int](0)
      case 2 => Array[Int](-1, 0)
      case odd if odd % 2 == 1 => Range(-((odd-1)/2), (odd-1) / 2).inclusive.toArray
      case even => Range(-even / 2, even / 2).toArray
    }




//    def regionEnhanceContrast(i: Image[Color], width: Int, height: Int): Color = {
//    val samples = for {
//      x <- Range(0, width, 1)
//      y <- Range(0, height, 1)
//    } yield (x, y)
//    val c = if (i.im(Loc))
//  }

  def circle(x: Float, y: Float, radius: Float): Image[Boolean] = {
    import Math.pow
    val circleProgram: Loc=>Boolean = { loc =>
      Color.aproxEq(
        (pow(loc.x - x, 2) + pow(loc.y - y, 2)).toFloat,
        pow(radius, 2).toFloat,
        175
      )
    }

    imApplicative.ap(imApplicative.pure(circleProgram))(self)
  }

  def addBlackBorder(c: Color): Image[Color] = {
    new Image[Color](loc =>
      if (loc.x <= 10 || loc.y <= 10 || loc.x >= 630 || loc.y >= 416)
        new Color(0f, 0f, 0f, c.alpha)
      else c
    )
  }

  def redCircle(bg: Color, x: Float, y: Float, radius: Float): Image[Color] = {
    import Math.pow
    val circleProgram: Loc=> Color = { loc =>
      if (Color.aproxEq(
        (pow(loc.x - x, 2) + pow(loc.y - y, 2)).toFloat,
        pow(radius, 2).toFloat,
        300
      )) Color.Red else bg
    }

    new Image[Color](loc => circleProgram(loc))

//    imApplicative.ap(imApplicative.pure(circleProgram))(self)
  }
  def stripes(c: Color, bg: Color): Image[Color] =
    new Image( loc =>
      if (loc.x % 10 == 0) c else bg
    )

  def topHalf(c: Color, bg: Color): Image[Color] =
    new Image( loc =>
      if (loc.y < 200) c else bg
    )
}


class ImageC(img: Loc => Color) extends Image[Color](img)

