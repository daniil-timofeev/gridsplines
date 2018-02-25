package piecewise
import org.scalacheck.Prop._
import org.scalacheck._
import piecewise.PiecewiseGen._

object AreaChecks extends Properties("Spline area test"){

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

}
