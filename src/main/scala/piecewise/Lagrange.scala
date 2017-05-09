package piecewise

import com.twitter.algebird.Interval.InLowExUp
import PieceFunction._
/**
  * Created by Даниил on 16.03.2017.
  */
abstract class Lagrange extends Polynomial{

  override def apply(x: Double) = ruleOfHorner(x, coefs: _*)

  override def derivative(x: Double) = ruleOfHorner(x, coefs.drop(1).zipWithIndex map(t => t._1 * (t._2 + 1)): _*)

}
