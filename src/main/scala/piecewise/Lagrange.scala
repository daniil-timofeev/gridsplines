package piecewise

import piecewise.PieceFunction._
/**
  *
  */
abstract class Lagrange extends Polynomial{

  override def apply(x: Double) = ruleOfHorner(x, coefs: _*)

  override def derivative(x: Double) =
    ruleOfHorner(x, coefs.drop(1).zipWithIndex map(t => t._1 * (t._2 + 1)): _*)

}
