package piecewise

import com.twitter.algebird.Interval.InLowExUp
import PieceFunction._
/**
  * Created by Даниил on 16.03.2017.
  */
abstract class Lagrange(override val interval: InLowExUp[Double]) extends Polynomial(interval){

  override def apply(x: Double) = ruleOfGorner(x, coefs: _*)

  override def derivative(x: Double) = ruleOfGorner(x, coefs.drop(1).zipWithIndex map(t => t._1 * (t._2 + 1)): _*)


}
