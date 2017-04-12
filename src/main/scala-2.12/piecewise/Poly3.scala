package piecewise

import com.twitter.algebird.Interval.InLowExUp

/** Cubic piecewise polynomial
  *
  */
trait Poly3{
  this: Polynomial =>
  assert(coefs.length == 4, "Cubic polynomial must have 4 coefficients")

  override def apply(x: Double): Double = PieceFunction.cubicRuleOfHorner(x, coefs(0), coefs(1), coefs(2), coefs(3))

  override def derivative(x: Double): Double = PieceFunction.cubicHornerDerivative(x, coefs(0), coefs(1), coefs(2), coefs(3))

  override def integral(x: Double): Double = PieceFunction.cubicHornerIntegral(x, coefs(0), coefs(1), coefs(2), coefs(3))
}
