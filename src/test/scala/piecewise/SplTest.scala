package piecewise
import org.scalacheck.util._
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import piecewise.SplineTest.{listGen, property}
import GenPiecewice.anyPoints

object SplTest extends Properties(" Spline tests"){


  implicit val arb = Arbitrary(anyPoints)

  property(" correct slicing * 2") =
    forAll{(vals: List[(Double, Double)]) =>
      val spline = Spline.lines(vals)
      val src1 = spline.sources
      val splitted = spline.splitWhere((_, _, _) => 2)
      val src2 = splitted.sources
      src2.size ?= src1.size * 2
    }


}
