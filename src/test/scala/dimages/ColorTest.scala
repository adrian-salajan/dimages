package dimages

import cats.kernel.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{ApplicativeTests, FunctorTests}
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline


class ColorTest extends AnyFunSuiteLike with FunSuiteDiscipline with Checkers {


  implicit val eqC: Eq[Color] = Eq.fromUniversalEquals

  import org.scalacheck.Arbitrary
  import org.scalacheck._

  implicit val arbColor: Arbitrary[Color] = Arbitrary(for {
    r <- Gen.choose(0f, 1f)
    g <- Gen.choose(0f, 1f)
    b <- Gen.choose(0f, 1f)
    a <- Gen.choose(0f, 1f)
  } yield  {

    val c = Color(r, g, b, a)
    c
  })

  implicit val config = generatorDrivenConfig.copy(minSuccessful = PosInt(100))
  checkAll("Color Min.MonoidLaws", MonoidTests(Color.colorMonoidMin).monoid)
  checkAll("Color Max .MonoidLaws", MonoidTests(Color.colorMonoidMax).monoid)
  checkAll("Color Conal .MonoidLaws", MonoidTests(Color.colorMonoidConal).monoid)

//  checkAll("Color Conal .MonoidLaws", FunctorTests(Image.imFunctor).functor[Int, Int, String])

//  checkAll("Color Conal .MonoidLaws", ApplicativeTests(Image.imApplicative).applicative)


}
