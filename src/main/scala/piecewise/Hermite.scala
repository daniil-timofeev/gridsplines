package piecewise
import com.twitter.algebird.Interval.InLowExUp

import scala.math.{abs, pow, signum}

/**
  * Created by Даниил on 19.02.2017.
  */
abstract class Hermite extends Polynomial with Slicer{

  val x0: Double

  override def apply(x: Double): Double = PieceFunction.cubicRuleOfHorner(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def derivative(x: Double): Double = PieceFunction.cubicHornerDerivative(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def integral(x: Double): Double = PieceFunction.cubicHornerIntegral(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))


  private lazy val body = f"*(x${-x0}%+.7f)"

  override lazy val toString = {
    f"${coefs(3)}%1.4f" + body + f"^3 ${coefs(2)}%+1.4f" + body + f"^2  ${coefs(1)}%+1.4f" +
      body + f" ${coefs(0)}%+1.4f"
  }


}
object Hermite{


}

