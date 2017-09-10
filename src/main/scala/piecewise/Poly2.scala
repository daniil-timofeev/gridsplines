package piecewise

/** Quadratic piecewise polynomial
  *
  */
trait Poly2 extends{
  this: Polynomial =>
  assert(coefs.length == 3, "Quadratic polynomial must have 3 coefficients")

  override def apply(x: Double): Double = PieceFunction.quadraticRuleOfHorner(x, coefs(0), coefs(1), coefs(2))

  override def derivative(x: Double): Double = PieceFunction.quadraticHornerDerivative(x, coefs(0), coefs(1), coefs(2))

  override def integral(x: Double): Double = PieceFunction.quadraticHornerIntegral(x, coefs(0), coefs(1), coefs(2))

}
