package piecewise

import com.twitter.algebird.Interval
import com.twitter.algebird.Interval.InLowExUp

/**
  * Created by Даниил on 18.03.2017.
  */
abstract class Polynomial(override val interval: InLowExUp[Double]) extends PieceFunction(interval){
  protected val coefs: Array[Double]
}
