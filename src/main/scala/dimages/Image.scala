package dimages


import java.awt.image.BufferedImage

import cats.kernel.Monoid

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

  def apply(b: BufferedImage): ImageC = new ImageC({
    loc: Loc => {

      Try(//map out of bounds to black
        {
          val c = new java.awt.Color(b.getRGB(loc.x.toInt, loc.y.toInt))
          Color(c.getRed.toFloat / 255, c.getGreen.toFloat / 255, c.getBlue.toFloat / 255, c.getAlpha.toFloat / 255)
        }
      ).fold(_ => Color.Black, c => c)
    }
  })

  def monochrome[A](a: A): Image[A]= Image.lift(a)

//  def imMonoid[A](implicit M: Monoid[A]): Monoid[Image[A]] = new Monoid[Image[A]] {
//    override def empty: Image[A] = new Image[A]({_ => M.empty})
//
//    override def combine(x: Image[A], y: Image[A]): Image[A] = new Image[A]({
//      loc => M.combine(x.im(loc), y.im(loc))
//    })
//  }
}

class ImageC(img: Loc => Color) extends Image[Color](img)

