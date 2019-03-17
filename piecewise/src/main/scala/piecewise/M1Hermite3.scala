package piecewise

import scala.collection.mutable.ListBuffer
import scala.math._

/** Monotonic piecewise cubic curve for the spline interpolation
  *
  * A Сurve, that serve for the approximation physical reality definitions, where monotonic property required
 *
  * @see Fritsch, F. N. Monotone piecewise cubic interpolation
  *      / F. N. Fritsch, R. E. Carlson // SIAM J. Numer. Anal. — 1980. — 17. № 2. — pp. 238 — 246.
  * @version 0.5.0
  * @author Даниил
  */
case class M1Hermite3(coefs: Array[Double], x0: Double) extends Hermite {


  override def apply(x: Double): Double = PieceFunction.cubicRuleOfHorner(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def derivative(x: Double): Double = PieceFunction.cubicHornerDerivative(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def antider(x: Double): Double = PieceFunction.cubicHornerIntegral(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  private lazy val body = f"*(x${-x0}%+.7f)"

  override lazy val toString = {
    f"${coefs(3)}%1.4f" + body + f"^3 ${coefs(2)}%+1.4f" + body + f"^2  ${coefs(1)}%+1.4f" +
      body + f" ${coefs(0)}%+1.4f"
  }

  //TODO get desired spline smoothness
  //private[this] lazy val fi4 = if(2 * alpha + beta < 3.0 || alpha + 2 * beta < 3.0) true else false

 // private[this] lazy val fi3 = if(alpha + beta < 3 || fi4 == false) true else false

 // private[this] lazy val fi2 = if(sqrt(pow(alpha,2) + pow(beta,2)) < 3.0 || fi3 == false) true else false

 // private[this] lazy val fi1 = if(alpha < 0.3 && beta < 0.3 || fi2 == false) true else false

  /** Spline smoothness
    *
    * @return
    *  string with "Smooth", "Normal", "Coarse", "Coarsest", which depends on curve smoothness
    */
 // def smoothness = {
  //  true match {
  //    case `fi1` => M1Hermite3 SMOOTH
  //    case `fi2` => M1Hermite3 NORMAL
  //    case `fi3` => M1Hermite3 COARSE
  //    case `fi4` => M1Hermite3 COARSEST
  //    case _ if isMonotone => "Monotone"
 //     case _ => "No monotone"
 //   }
 // }

  /** Extremum of the function `x`
    *
    * @return extremums of function */

  override protected def extremum(low: Double, upp: Double): List[Double] = ???

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case sameType: Hermite3 => {
        coefs.zip(sameType.coefs).forall(t => t._1.equals(t._2)) &&
          x0.equals(sameType.x0)
      }
      case _ => false
    }
  }

  override def area(x0: Double, x1: Double) = ???
}
object M1Hermite3 {

  def constructSpline(source: Array[Double]): ((Double, Double), M1Hermite3) = {
    val Array(yLow, yUpp, sdLow, sdUpp, xLow, xUpp) = source
    val delta_ = delta(yLow, yUpp, xLow, xUpp)
    val h_ = h(xLow, xUpp)
    val coefs: Array[Double] = Array(
        yLow, sdLow,
        (-2.0 * sdLow - sdUpp + 3.0 * delta_) / h_,
        (sdLow + sdUpp - 2.0 * delta_) / pow(h_, 2.0)
    )
    ((xLow, xUpp), M1Hermite3(coefs, xLow))
  }

  def smoothness(prev: Array[Double],
                 next: Array[Double])
                     : Array[Double] = {
    val Array(_, _, _, dLeft, _, _) = prev
    val Array(_, _, dRight, _, _, _) = next
    val der = signum(dLeft) * min(abs(dRight), abs(dLeft))
    prev.update(3, der)
    next.update(2, der)
    prev
  }

  def apply(values: Iterator[(Double, Double)]
           ): Iterator[((Double, Double), M1Hermite3)] = {
    import Hermite3._
    val vals = values.toList
    val sources = Hermite3.makeSources(vals, deriv(vals.head, vals.tail.head))
      .map(monothone(_)(Normal))

    if (sources.isEmpty) Iterator.empty
    else {

      def go(prevous: Array[Double],
             iter: Iterator[Array[Double]],
             acc: ListBuffer[Array[Double]]): Iterator[Array[Double]] = {
        if (iter.isEmpty) {
          acc += prevous
          acc.result().iterator
        } else {
          val next = iter.next()
          val transformed = smoothness(prevous, next)
          acc += transformed
          go(next, iter, acc)
        }
      }

      val first = sources.next()
      go(first, sources, ListBuffer.empty).map(constructSpline)
    }
  }



  def apply(x: List[Double], y: List[Double]): List[M1Hermite3] = {
      ???
  }

  implicit def convert[S <: PieceFunction](low: Double,
                                           upp: Double, fn: S): M1Hermite3 = {
    fn match {
      case line: Line => new M1Hermite3(Array(line.intercept, line.slope, 0.0, 0.0), 0.0)
      case nonMonothone: Hermite3 => ???
    }
  }

}
