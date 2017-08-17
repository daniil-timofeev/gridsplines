package piecewise
import org.scalacheck.util._
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import piecewise.SplineTest.{listGen, property}

object SplTest extends Properties(" Spline tests"){

  val xCoords: Gen[Set[Double]] =
  nonEmptyListOf(arbDouble.arbitrary).map(_.toSet).suchThat(set => set.size > 3)

  val points = for{
    xVals: Set[Double] <- xCoords
  } yield xVals.toList.zip(xVals.toList.map(x => arbDouble.arbitrary.sample.get))

  implicit val arb = Arbitrary(points)

  property(" correct slicing * 2") =
    forAll{(vals: List[(Double, Double)]) =>
      val spline = Spline.lines(vals)
      val src1 = spline.sources
      val splitted = spline.splitWhere((_, _, _) => 2)
      val src2 = splitted.sources
      src2.size ?= src1.size * 2
    }


}
