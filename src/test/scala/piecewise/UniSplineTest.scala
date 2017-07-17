package piecewise

import org.scalacheck.Gen.{choose, nonEmptyListOf}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._
import org.scalacheck.Prop.AnyOperators
/**
  * Created by Даниил on 14.07.2017.
  */
object UniSplineTest extends Properties("UniSpline Test"){

  val listGen: Gen[List[(Double, Double)]] = nonEmptyListOf[(Double, Double)](
    for{
      x <- Arbitrary.arbDouble.arbitrary
    } yield (x, 10.0)) suchThat(list => list.size >= 3)


  implicit val listArb = Arbitrary(listGen)
  implicit val d = Arbitrary.arbDouble

  property("Defined at all interval") =
    forAll{(points: List[(Double, Double)], x: Double) =>
      val sorted = points.sortBy(_._1)
      val spline = UniSpline[Line](sorted)
      val (xLow, xUpp, _, _) = Spline.boundsOf(spline)
      all(
        spline(x) == 10.0
      )
  }


}
