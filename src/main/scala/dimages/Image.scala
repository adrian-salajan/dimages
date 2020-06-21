package dimages


import java.awt.image.BufferedImage

import cats.kernel.Monoid
import cats.{Applicative, Comonad, Functor, Monad}

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
          Color(c.getRed.toFloat / 255, c.getGreen.toFloat / 255, c.getBlue.toFloat / 255, c.getAlpha.toFloat / 255)
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

  implicit val imApplicative: Applicative[Image] = new Applicative[Image] {
    override def pure[A](x: A): Image[A] = Image.lift(x)

    override def ap[A, B](ff: Image[A => B])(fa: Image[A]): Image[B] =
      Image.lift2[A => B, A, B] {
        fab => a => fab(a)
      }(ff)(fa)

    def mapp[A, B](fa: Image[A])(f: A => B): Image[B] =
      ap(pure(f))(fa)

    def ap22[A, B](ff: Image[A => B])(fa: Image[A]): Image[B] =
      map2(ff, fa)((f, a) => f(a))

    def map22[A, B, C](fa: Image[A], fb: Image[B], f: (A, B) => C): Image[C] = {
      val one = ap(pure(f.curried))(fa)
      ap(one)(fb)
    }

  }
  implicit val imMonad: Monad[Image] = new Monad[Image] {
    override def flatMap[A, B](fa: Image[A])(f: A => Image[B]): Image[B] = new Image[B]({
      loc =>
        val img: Image[B] = f(fa.im(loc))
        img.im(loc)
    })

    override def tailRecM[A, B](a: A)(f: A => Image[Either[A, B]]): Image[B] = new Image[B]({
      loc =>
        def rec(ab: Either[A, B]): B = ab match {
          case Left(a) => rec(f(a).im(loc))
          case Right(b) => b
        }

        rec(f(a).im(loc))

      //        f(a).im(loc) match {
      //          case Left(a) =>
      //            val x  = tailRecM(a)(f).im(loc)
      //            x
      //          case Right(b) => b
      //        }
    })


    override def pure[A](x: A): Image[A] = Image.lift(x)

    //    def flatMapp[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(map(fa, f))
    //
    //    def flatten2[F[_], A](ffa: F[F[A]]): F[A] = flatMapp(ffa)(a => a)
  }

  private implicit val imComonad = new Comonad[Image] {
    override def extract[A](x: Image[A]): A = x.im(Loc(0, 0))

    override def coflatMap[A, B](fa: Image[A])(f: Image[A] => B): Image[B] = {
      val a = f(fa)
      new Image[B]({
        loc => a
      })
    }

    override def map[A, B](fa: Image[A])(f: A => B): Image[B] = imFunctor.map(fa)(f)

    override def coflatten[A](fa: Image[A]): Image[Image[A]] = new Image[Image[A]]({
      loc =>
        new Image[A]({loc2 => fa.im(loc2)})
    })
  }

  def avg(i: Image[Color]): Color = {
    val samples = for {
      x <- Range(0, 500, 5)
      y <- Range(0, 500, 5)
    } yield (x, y)
    val sampleSize = samples.size
    val total = samples.map(n => i.im(Loc(n._1, n._2))).reduce((a, b) => a +! b)
    Color(total.red / sampleSize, total.green / sampleSize, total.blue / sampleSize)
  }

  def brightest(i: Image[Color]): Color = {
    val samples = for {
      x <- Range(0, 500, 5)
      y <- Range(0, 500, 5)
    } yield (x, y)
    val max = samples.map(n => i.im(Loc(n._1, n._2))).reduce((a, b) => if (a.brightness > b.brightness) a else b)
    max
  }

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
}

class ImageC(img: Loc => Color) extends Image[Color](img)

