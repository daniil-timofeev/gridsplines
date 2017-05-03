import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.{InclusiveLower, Intersection, Lower, Upper}

/**
  * Created by Даниил on 18.03.2017.
  */
package object piecewise {

  class additionInterval(val interval: InLowExUp[Double]) extends AnyVal{
      def length = interval.upper.upper - interval.lower.lower
  }

  @inline
  def cubicRoots(a: Double, b: Double, c: Double, d: Double): Seq[Double] = {
    import scala.math.pow
    val p = pow(b, 2.0) - 3 * a * c
    val q = 9 * a * b * c - 2 * pow(b, 3.0) - 27.0 * pow(a, 3.0) * d
    val n = 27.0 * pow(p, 3.0) / pow(q, 2.0)
    if (n > 6.75) {
      val theta = 1.0 / math.tan(math.sqrt(4 * n / 27.0 - 1.0))
      val cosin = (x: Double) => p * (4 * pow(math.cos(theta / 3.0 + x), 2.0) - 3.0)
      val x1 = (-b + q / cosin(0)) / (3.0 * a)
      val x2 = (-b + q / cosin(120 / scala.math.Pi)) / (3.0 * a)
      val x3 = (-b + q / cosin(240 / scala.math.Pi)) / (3.0 * a)
      Seq(x1, x2, x3)
    }
    else {
      val root = math.sqrt(pow(q, 2.0) / 4.0 - pow(p, 3.0))
      val n = math.cbrt(q / 2.0 + root) + math.cbrt(q / 2.0 - root)
      Seq((-b + n) / (3.0 * a))
    }

  }


  @inline
  def cubicRootsVieta(a: Double, b: Double, c: Double): Seq[Double] = {
    import scala.math._
    val q = (math.pow(a, 2.0) - 3.0 * b) / 9.0
    val r = (2 * pow(a, 3.0) - 9.0 * a * b + 27.0 * c) / 54.0
    val m = pow(r, 2.0) - pow(q, 3.0)

    if (m < 0.0) {
      val fi = acos(r / sqrt(pow(q, 3.0)))
      val x1 = -2.0 * sqrt(q) * cos(fi / 3.0) - a / 3.0
      val x2 = -2.0 * sqrt(q) * cos((fi + 2.0 * math.Pi) / 3.0) - a / 3.0
      val x3 = -2.0 * sqrt(q) * cos((fi - 2.0 * math.Pi) / 3.0) - a / 3.0
      Seq(x2, x1, x3)
    }
    else {
      val s = cbrt(-r + sqrt(m))
      val t = cbrt(-r - sqrt(m))
      Seq(s + t - a / 3.0)
    }
  }



}
