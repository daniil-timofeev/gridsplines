import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.{InclusiveLower, Intersection, Lower, Upper}

/**
  * Created by Даниил on 18.03.2017.
  */
package object piecewise {

  class additionInterval(val interval: InLowExUp[Double]) extends AnyVal{
      def length = interval.upper.upper - interval.lower.lower
  }



}
