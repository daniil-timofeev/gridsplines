package piecewise
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

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
case class M1Hermite3(protected val yL: Double, protected val yUp: Double, protected val dL: Double, protected val dUp: Double,
                      override protected val low: Double, override protected val upp: Double) extends Hermite(low, upp) with Poly3{

  type SliceType = M1Hermite3

  //TODO get desired spline smoothness
  private[this] lazy val fi4 = if(2 * alpha + beta < 3.0 || alpha + 2 * beta < 3.0) true else false

  private[this] lazy val fi3 = if(alpha + beta < 3 || fi4 == false) true else false

  private[this] lazy val fi2 = if(sqrt(pow(alpha,2) + pow(beta,2)) < 3.0 || fi3 == false) true else false

  private[this] lazy val fi1 = if(alpha < 0.3 && beta < 0.3 || fi2 == false) true else false

  /** Гладкость сплайна / Spline smoothness
    *
    * @return строку со значениями "Smooth", "Normal", "Coarse", "Coarsest", в зависимости от гладкости /
    *         string with "Smooth", "Normal", "Coarse", "Coarsest", in dependence of curve smoothness
    */
  def smoothness = {
    true match {
      case `fi1` => M1Hermite3 SMOOTH
      case `fi2` => M1Hermite3 NORMAL
      case `fi3` => M1Hermite3 COARSE
      case `fi4` => M1Hermite3 COARSEST
      case _ if isMonotone => "Monotone"
      case _ => "No monotone"
    }
  }

  def sliceUpper(upper: Double): SliceType = this.copy(yUp = apply(upper), dUp = derivative(upper), upp = upper)

  def sliceLower(lower: Double): SliceType = this.copy(yL = apply(lower), dL = derivative(lower), low = lower)

  private def fi = alpha - 1.0 / 3.0 * pow(2.0 * alpha + beta - 3.0,2.0)/(alpha + beta - 2.0)
}
object M1Hermite3 {

  implicit val SMOOTH: String = "Smooth"
  implicit val NORMAL: String = "Normal"
  implicit val COARSE: String = "Coarse"
  implicit val COARSEST: String = "Coarsest"

  def smooth(splines: Vector[M1Hermite3]): Vector[M1Hermite3] = {
    if(splines.size == 1) splines.toVector
    else{
      val b = Vector.newBuilder[Double]
      /* На границе */
      val (lowBound, upBound) = (splines.head.dL, splines.last.dUp)
      b += lowBound
      b += upBound
      /* Производные */

      (splines.view, splines drop 1).zipped.map{(spl1, spl2) => min(spl1.dUp, spl2.dL)}.foreach(b += _)
      val derivatives = b.result()
      (splines, derivatives, derivatives drop 1).zipped.map{(spline, dd, dd1) => {
        val copied = spline.copy(dL = dd, dUp = dd1)
        spline match {
          case spl if copied.isMonotone => copied
          case spl if spl.isMonotone => spl
          case spl if spl.copy(dUp = dd1).isMonotone => spl.copy(dUp = dd1)
          case spl  if spl.copy(dL = dd).isMonotone => spl.copy(dL = dd)
          case spl => spl
        }
      }}
  }}

  def apply(values: List[(Double, Double)])(implicit mType: String = COARSE): Vector[M1Hermite3] = {
    val splines = Hermite3(values)
    smooth(splines.map(spl => spl.monotone(mType)))
  }

  def apply(x: List[Double], y: List[Double]): Vector[M1Hermite3] = {
    val splines = Hermite3(x, y)
    smooth(splines.map(spl => spl.monotone(COARSE)))
  }

}
