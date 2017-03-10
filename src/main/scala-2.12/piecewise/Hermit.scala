package piecewise
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

import scala.math.{abs, pow, signum}

/**
  * Created by Даниил on 19.02.2017.
  */
abstract class Hermit(interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) extends PieceFunction(interval){

  val yL, yUp, dL, dUp : Double

  if({yL :: yUp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные " +
    " значения функции должны быть вещественными числами / initial values of function must be not NaN")

  if({dL :: dUp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные" +
    " значение производных должны быть вещественными числами / initial values of derivatives must be not NaN")

  if({interval.lower.lower :: interval.upper.upper :: Nil} exists(_.isNaN)) throw
    new IllegalArgumentException(" Исходные" +
      " значения аргументнов должны быть вещественными числами / initial values of arguments must be not NaN")

  protected lazy val h = interval.upper.upper - interval.lower.lower

  private def fi = alpha - 1.0/3.0*pow(2*alpha + beta - 3.0,2.0)/(alpha + beta - 2.0)
  protected val delta = (yUp - yL) / h

  private val a: Double = (dL + dUp - 2 * delta) / pow(h, 2.0)

  private val b: Double = (-2 * dL - dUp + 3 * delta) / h

  protected lazy val alpha  = abs(dL / delta)

  protected lazy val beta = abs(dUp / delta)

  lazy val extremum: List[Double] = List(interval.lower.lower + h / 3.0 * (2 * alpha + beta - 3.0)/(alpha + beta - 2))

  lazy val isMonotone = {
    if(alpha + beta - 2.0 >= 0 && (signum(dL) == signum(dUp) || dL == 0 || dUp == 0)){
      if(2 * alpha + beta - 3 <= 0.0 || alpha + 2 * beta - 3.0 <= 0 || fi >= 0) true else false
    } else false
  }

  private[this] val l = interval.lower.lower

  override def apply(x: Double): Double = a * pow(x - l, 3) +
    b * pow(x - l, 2) + dL * (x - l) + yL

  override def derivative(x: Double): Double = 3 * a * pow(x - l, 2) +
    2 * b * pow(x - interval.lower.lower, 2) + dL

  override def integral(x: Double): Double = 0.25 * a * pow(x - l, 4) + 1.0 / 3.0 * b * pow(x - l, 3) +
    0.5 * dL * pow(x - l, 2) + yL * (x - l)

  private lazy val body = f"*(x${-interval.lower.lower}%+.7f)"

  override lazy val toString = {
    f"$a%1.4f" + body + f"^3 $b%+1.4f" + body + f"^2  $dL%+1.4f" +
      body + f" $yL%+1.4f"
  }


}
