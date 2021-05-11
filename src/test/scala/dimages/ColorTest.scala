package dimages

import cats.kernel.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{ApplicativeTests, ComonadTests, ExhaustiveCheck, FunctorTests, MonadTests}
import org.scalacheck.ScalacheckShapeless.derivedCogen
import org.scalactic.anyvals.PosInt
import org.scalatest.Assertions
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

    implicit val arbImageIntXY: Arbitrary[Imagexy[Int]] = Arbitrary(
    Arbitrary.arbInt.arbitrary.map(in => new Imagexy[Int]({ loc => in + (loc.x + loc.y).toInt}, Loc(in, in)))
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
  implicit val stringeq: Eq[String] = Eq.apply

  val doubles: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val locations = for{
    a <- doubles
    b <- doubles
  } yield Loc(a, b)


  implicit val exCLoc: ExhaustiveCheck[Loc] = ExhaustiveCheck.instance(locations)

  implicit def eqT[T]: Eq[T] = Eq.fromUniversalEquals[T]
  implicit  def eqLocT[T]: Eq[Loc => T] = catsLawsEqForFn1Exhaustive[Loc, T]
  implicit  def eqImgT[T]: Eq[Image[T]] = Eq.by[Image[T], Loc => T](img => img.im)
//  implicit  def eqImgT2[T]: Eq[Image[T]] = Eq.instance()

//  checkAll("Functor Laws", FunctorTests(Image.imFunctor).functor[Int, Int, String])

//  checkAll("Applicative laws", ApplicativeTests(Image.imApplicative).applicative[Int, Int, String])

//  implicit val arbImageColor: Arbitrary[Image[Color]]
//  checkAll("Monad laws", MonadTests(Image.imMonad).monad[Int, Int, String])

  implicit def cogenImage[A]: Cogen[Image[A]] =Cogen(_ => 3)
//  checkAll("CoMonad laws", ComonadTests(Image.imComonad).comonad[Int, Int, String])
implicit def cogenImageXY[A]: Cogen[Imagexy[A]] = Cogen(a => (a.focus.x + a.focus.y).toLong)
  checkAll("CoMonad laws XY", ComonadTests(Imagexy.imComonadxy).comonad[Int, Int, String])

  test("comonad Left identity") {
    val fa = new Image[Loc](identity)
    val fb = Image.imComonad.coflatMap(fa)(Image.imComonad.extract)

    val areEqual = for {
      x <- 0 to 10
      y <- 0 to 10
    } yield fa.im(Loc(x, y)) == fb.im(Loc(x, y))

    assert(!areEqual.contains(false))
  }

    test("comonad Right identity") {
    val fa = new Image[Loc](identity)
    val f37 = (a: Image[Loc]) => a.im(Loc(3, 7))
    val fb = Image.imComonad.extract(Image.imComonad.coflatMap(fa)(f37))
    val p37 = f37(fa)

    assertResult(p37)(fb)
  }

//   fa.coflatten <-> fa.coflatMap(identity)
  test("coflatten identity") {
    import Image.imComonad
    import cats.implicits._
    val fa = new Image[Loc](identity)

    val ffa = fa.coflatten
    val ffa2 = fa.coflatMap(identity)

    val areEqual = for {
      x <- 0 to 10
      y <- 0 to 10
      loc = Loc(x, y)
    } yield ffa.im(loc).im(loc) == ffa2.im(loc).im(loc)

     assert(!areEqual.contains(false))
  }

  test("coflatten through map") {
    import Image.imComonad
    import cats.implicits._
    val fa = new Image[Loc](identity)
    val ffa = fa.coflatten.coflatten
    val ffaViaMAp = fa.coflatten.map(_.coflatten)

     val areEqual = for {
      x <- 0 to 10
      y <- 0 to 10
      loc = Loc(x, y)
    } yield ffa.im(loc).im(loc).im(loc) == ffaViaMAp.im(loc).im(loc).im(loc)

     assert(!areEqual.contains(false))
  }

//----

  test("2comonad Left identity") {
    val fa = new Imagexy[Loc](identity)
    val fb = Imagexy.imComonadxy.coflatMap(fa)(Imagexy.imComonadxy.extract)

    val areEqual = for {
      x <- 0 to 10
      y <- 0 to 10
    } yield fa.im(Loc(x, y)) == fb.im(Loc(x, y))

    assert(!areEqual.contains(false))
  }

    test("2comonad Right identity") {
    val fa = new Imagexy[Loc](identity)
    val f37 = (a: Imagexy[Loc]) => a.im(Loc(3, 7))
    val fb = Imagexy.imComonadxy.extract(Imagexy.imComonadxy.coflatMap(fa)(f37))
    val p37 = f37(fa)

    assertResult(p37)(fb)
  }

//   fa.coflatten <-> fa.coflatMap(identity)
  test("2coflatten identity") {
    import Image.imComonad
    import cats.implicits._
    val fa = new Imagexy[Loc](identity)

    val ffa = fa.coflatten
    val ffa2 = fa.coflatMap(identity)

    val areEqual = for {
      x <- 0 to 10
      y <- 0 to 10
      loc = Loc(x, y)
    } yield ffa.im(loc).im(loc) == ffa2.im(loc).im(loc)

     assert(!areEqual.contains(false))
  }

  test("2coflatten through map") {
    import Image.imComonad
    import cats.implicits._
    val fa = new Imagexy[Loc](identity)
    val ffa = fa.coflatten.coflatten
    val ffaViaMAp = fa.coflatten.map(_.coflatten)

     val areEqual = for {
      x <- 0 to 10
      y <- 0 to 10
      loc = Loc(x, y)
    } yield ffa.im(loc).im(loc).im(loc) == ffaViaMAp.im(loc).im(loc).im(loc)

     assert(!areEqual.contains(false))
  }


}
