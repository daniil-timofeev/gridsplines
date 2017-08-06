package piecewise
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

import scala.collection.mutable.ListBuffer
import scala.math._

/** Монотонная кусочная кубическая кривая для интерполяции / Monotonic piecewise cubic curve for interpolation
  * Кривая, предназначеная для аппроксимации явлений физической реальности, где требуется монотонность
  * / Сurve, that serve for approximation physical reality definitions, where monotonic property required
 *
  * @see Fritsch, F. N. Monotone piecewise cubic interpolation
  *      / F. N. Fritsch, R. E. Carlson // SIAM J. Numer. Anal. — 1980. — 17. № 2. — pp. 238 — 246.
  * @version 0.5.0
  * @author Даниил
  */
case class M1Hermite3(coefs: Array[Double], x0: Double) extends Hermite {

  type SliceType = M1Hermite3

  //TODO get desired spline smoothness
  //private[this] lazy val fi4 = if(2 * alpha + beta < 3.0 || alpha + 2 * beta < 3.0) true else false

 // private[this] lazy val fi3 = if(alpha + beta < 3 || fi4 == false) true else false

 // private[this] lazy val fi2 = if(sqrt(pow(alpha,2) + pow(beta,2)) < 3.0 || fi3 == false) true else false

 // private[this] lazy val fi1 = if(alpha < 0.3 && beta < 0.3 || fi2 == false) true else false

  /** Гладкость сплайна / Spline smoothness
    *
    * @return строку со значениями "Smooth", "Normal", "Coarse", "Coarsest", в зависимости от гладкости /
    *         string with "Smooth", "Normal", "Coarse", "Coarsest", in dependence of curve smoothness
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

  def sliceUpper(upper: Double): SliceType = this

  def sliceLower(lower: Double): SliceType = this

  /** Экстремум функции `x`
    * Extremum of function `x`
    *
    * @return экстремумы функции / extremums of function */

  override protected def extremum: List[Double] = ???

}
object M1Hermite3 {


  def constructSpline(source: Array[Double]): M1Hermite3 = {
    val Array(yLow, yUpp, sdLow, sdUpp, xLow, xUpp) = source
    val delta = Hermite3.delta(yLow, yUpp, xLow, xUpp)
    val h = Hermite3.h(xLow, xUpp)
    val coefs: Array[Double] = Array(
        yLow, sdLow,
        (-2.0 * sdLow - sdUpp + 3.0 * delta) / h,
        (sdLow + sdUpp - 2.0 * delta) / pow(h, 2.0)
    )
    M1Hermite3(coefs, xLow)
  }

  def smoothness(prev: Array[Double],
                 next: Array[Double])
                     : Array[Double] = {
    val Array(_, _, _, dLeft, _, _) = prev
    val Array(_, _, dRight, _, _, _) = next
    val der = signum(dLeft) * min(abs(dRight), abs(dLeft))
    prev.update(3, der)
    prev
  }

  def apply(values: List[(Double, Double)]): List[M1Hermite3] = {
    import Hermite3._
    val sources = Hermite3.makeSources(values)
      .map(monothone(_)(Normal))

    val buffer = ListBuffer.empty[M1Hermite3]

    var prevous = sources.next()
    if(sources.hasNext) {
      while (sources.hasNext) {
        val next = sources.next()
        val source = smoothness(prevous, next)
        buffer += constructSpline(source)
        if(sources.isEmpty) {
          next.update(2, source(3))
          buffer += constructSpline(next)
          }
        prevous = next
      }
    }
    else buffer += constructSpline(prevous)
    buffer.result()
  }

  def apply(x: List[Double], y: List[Double]): List[M1Hermite3] = {
      ???
  }

}
