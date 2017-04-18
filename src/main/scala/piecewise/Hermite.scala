package piecewise
import com.twitter.algebird.Interval.InLowExUp

import scala.math.{abs, pow, signum}

/**
  * Created by Даниил on 19.02.2017.
  */
abstract class Hermite(protected val low: Double, protected val upp: Double) extends Polynomial{

  val yL, yUp, dL, dUp : Double

  if({yL :: yUp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные " +
    " значения функции должны быть вещественными числами / initial values of function must be not NaN")

  if({dL :: dUp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные" +
    " значение производных должны быть вещественными числами / initial values of derivatives must be not NaN")

  if({low :: upp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные" +
      " значения аргументнов должны быть вещественными числами / initial values of arguments must be not NaN")

  protected lazy val h = upp - low

  private def fi = alpha - 1.0/3.0*pow(2.0*alpha + beta - 3.0,2.0)/(alpha + beta - 2.0)

  protected val delta = (yUp - yL) / h

  override val coefs: Array[Double] = Array(
    yL, dL,
    (-2.0 * dL - dUp + 3.0 * delta) / h,
    (dL + dUp - 2.0 * delta) / pow(h, 2.0)
  )

  protected lazy val alpha  = abs(dL / delta)

  protected lazy val beta = abs(dUp / delta)

  lazy val extremum: List[Double] = List(low + h / 3.0 * (2 * alpha + beta - 3.0)/(alpha + beta - 2))

  lazy val isMonotone = {
    if(alpha + beta - 2.0 >= 0 && (signum(dL) == signum(dUp) || dL == 0 || dUp == 0)){
      if(2 * alpha + beta - 3 <= 0.0 || alpha + 2 * beta - 3.0 <= 0 || fi >= 0) true else false
    } else false
  }


  private lazy val body = f"*(x${-low}%+.7f)"

  override lazy val toString = {
    f"${coefs(3)}%1.4f" + body + f"^3 ${coefs(2)}%+1.4f" + body + f"^2  $dL%+1.4f" +
      body + f" $yL%+1.4f"
  }

}
object Hermite{


}

