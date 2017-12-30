package piecewise
import org.scalacheck.Arbitrary.arbDouble
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._
import piecewise.GenPiecewiceCheck.anyPositivePoints

object RoughAreaChecks extends Properties("Spline area test"){


  val areaPointsGen = for{
                       points <- anyPositivePoints
                       lower <- choose(points.head._1, points.last._1)
                       upper <- choose(lower, points.last._1)
  } yield { (points, lower, upper) }

  implicit val arb = Arbitrary(areaPointsGen)
  implicit val atb0 = Arbitrary(anyPositivePoints)

  property("1") =
  forAllNoShrink{points: List[(Double, Double)] =>
    val lower = points.head._1
    val upper = points.last._1

    val max = points.maxBy(_._2)._2
    val min = points.minBy(_._2)._2
    val spline = Spline.lines(points).get
    val average = spline.average(lower, upper)
    all(
      s"Max ${max} is less than average ${average} " |:
      propBoolean(average <  max),
      s"Min ${min} is more than average ${average}" |:
      propBoolean(average >  min)
    )
  }

  property("2") =
  forAllNoShrink{data: (List[(Double, Double)], Double, Double) =>
    val (points, lower, upper) = data
    val spline = Spline.lines(points).get
    spline.average(lower, upper) > 0.0
  }



}
