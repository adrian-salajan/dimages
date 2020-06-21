package dimages

import cats.kernel.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{ApplicativeTests, ExhaustiveCheck, FunctorTests, MonadTests}
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import scala.Option


class ColorTest extends AnyFunSuiteLike with FunSuiteDiscipline with Checkers {

//wtf
  implicit val eqC: Eq[Color] = Eq.fromUniversalEquals

  import org.scalacheck.Arbitrary
  import org.scalacheck._

//  implicit val arbColor: Arbitrary[Color] = Arbitrary(for {
//    r <- Gen.choose(0f, 1f)
//    g <- Gen.choose(0f, 1f)
//    b <- Gen.choose(0f, 1f)
//    a <- Gen.choose(0f, 1f)
//  } yield  {

//    val c = Color(r, g, b, a)
//    c
//  })

  implicit val config = generatorDrivenConfig.copy(minSuccessful = PosInt(100))
//  checkAll("Color Min.MonoidLaws", MonoidTests(Color.colorMonoidMin).monoid)
//  checkAll("Color Max .MonoidLaws", MonoidTests(Color.colorMonoidMax).monoid)
//  checkAll("Color Conal .MonoidLaws", MonoidTests(Color.colorMonoidConal).monoid)

  implicit val arbImageInt: Arbitrary[Image[Int]] = Arbitrary(
    Arbitrary.arbInt.arbitrary.map(in => new Image[Int]({ loc => in + (loc.x + loc.y).toInt}))
  )
  implicit val arbImageString: Arbitrary[Image[String]] = Arbitrary(
    Arbitrary.arbInt.arbitrary.map(in => new Image[String]({ loc => in.toString +(loc.x + loc.y).toString}))
  )

  implicit val arbImageIntInt: Arbitrary[Image[Int => Int]] = Arbitrary(
    Arbitrary.arbInt.arbitrary.map(in => new Image[Int => Int]({ loc => x => in + x + (loc.x + loc.y).toInt}))
  )

  implicit val arbImageIntString: Arbitrary[Image[Int => String]] = Arbitrary(
    Arbitrary.arbInt.arbitrary.map(in => new Image[Int => String]({ loc => x => in.toString + x.toString + (loc.x + loc.y).toString}))
  )

  import cats.instances.all._
  import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
  implicit val inteq: Eq[Int] = Eq.apply
  implicit val stringeq: Eq[Int] = Eq.apply

  val doubles: List[Double] = List.unfold(0d)(s => if (s > 10d) Option.empty else Some((s + 0.4, s + 0.4)))
  val locations = for{
    a <- doubles
    b <- doubles
  } yield Loc(a, b)


  implicit val exCLoc: ExhaustiveCheck[Loc] = ExhaustiveCheck.instance(locations)

  implicit def eqT[T]: Eq[T] = Eq.instance((ta, tb) => ta == tb)
  implicit  def eqLocT[T]: Eq[Loc => T] = catsLawsEqForFn1Exhaustive[Loc, T]
  implicit  def eqImgT[T]: Eq[Image[T]] = Eq.by[Image[T], Loc => T](img => img.im)
  checkAll("Functor Laws", FunctorTests(Image.imFunctor).functor[Int, Int, String])

  checkAll("Applicative laws", ApplicativeTests(Image.imApplicative).applicative[Int, Int, String])

//  implicit val arbImageColor: Arbitrary[Image[Color]]
  checkAll("Monad laws", MonadTests(Image.imMonad).monad[Int, Int, String])


}
